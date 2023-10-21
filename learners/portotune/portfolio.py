import os
import logging
from collections import defaultdict

import numpy

from downward.reports import PlanningReport


TIMEOUT = 1800
EPSILON = 0.0001


def get_score(portfolio_file):
    with open(portfolio_file) as f:
        for line in f:
            if line.startswith('Score:'):
                return float(line.split(':')[1])
    assert False, 'Score could not be read from %s' % portfolio_file


def find_best_portfolios(path, n):
    portfolios = [os.path.join(path, filename) for filename in os.listdir(path)
                  if filename.endswith('.py')]
    portfolios = [(get_score(p), p) for p in portfolios]
    portfolios.sort(reverse=True)
    best = defaultdict(list)
    for score, portfolio in portfolios:
        filename, ext = os.path.splitext(os.path.basename(portfolio))
        parts = filename.split('-')
        generator = parts[0]
        if generator == 'fdss':
            generator = parts[1]
        if len(best[generator]) < n:
            best[generator].append(portfolio)
    assert len(best) == 7, best
    return best


def normalize_domain_name(name):
    removals = ['-', '_']
    removals += ['strips', 'adl', 'opt08', 'sat08']
    for item in removals:
        name = name.replace(item, '')
    return name


def same_domain(name1, name2):
    return normalize_domain_name(name1) == normalize_domain_name(name2)


class InvalidPortfolio(Exception):
    pass


class Portfolio(PlanningReport):
    def __init__(self, optimal=True, absolute_quality=False, plantime=TIMEOUT, **kwargs):
        self.plantime = plantime
        self.optimal = optimal
        PlanningReport.__init__(self, **kwargs)
        # Use absolute qualities instead of qualities normalized by the number
        # of problems per domain.
        self.absolute_quality = absolute_quality
        # Evaluates performance on a set of "num" training variations
        self.num_variations = None
        # Standard derivation of variations of total_time
        self.variations_stddev = kwargs.get('variations-stddev', 30)

        # for output information - this should be redefined by the subclasses
        self.portfolio_name = 'Fastr Portfolio'

        self.final_config = None

        # for the portfolio file - additional information can be added in the
        # subclasses
        self.settings = []
        self.settings.append('Maximum plantime: %i' % self.plantime)
        self.settings.append('Absolute quality: %s' % self.absolute_quality)
        if self.num_variations:
            self.settings.append('Number of variations: %i' %
                                 self.num_variations)
            self.settings.append('Standard deviation of the variations: %f' %
                                 self.variations_stddev)

        # for the portfolio report - this should be redefined by the subclasses
        self.report_descr = ('A portfolio for Fast Downward.')

    def write(self):
        if self.outfile.endswith('.html'):
            self.portfolio_file = self.outfile.replace('.html', '.py')
        else:
            self.portfolio_file = self.outfile + '.html'
        self.compute_portfolio()
        PlanningReport.write(self)
        self.print_portfolio()

    def _scan_data(self):
        PlanningReport._scan_data(self)

        self.commandline_configs = []
        self.total_times = [] # total_time for each problem-config pair
        self.costs = [] # cost for each problem-config pair
        self.qualities = [] # quality (ipc score) for each problem-config pair
         # True if the specific config was trained on the domain (resp. problem)
        self.trained = []

        self._retrieve_information()

        # Save a mapping from domain names to indices in self.problems
        self.domain_to_problem_indices = defaultdict(list)
        for prob_id, (domain, problem) in enumerate(self.problems):
            self.domain_to_problem_indices[domain].append(prob_id)

        # transform data matrices to numpy matrices for easier handling
        self.total_times = numpy.array(self.total_times)
        self.orig_qualities = numpy.array(self.qualities)
        self.trained = numpy.array(self.trained)
        # filter qualities of trained domains
        self.qualities = numpy.where(self.trained, 0, self.orig_qualities)

        # replace missing times with infty for the evaluator
        times = numpy.where(numpy.equal(self.total_times, None), numpy.inf,
                            self.total_times).astype('float')
        # define the evaluator
        if self.num_variations:
            self.evaluator = get_norm_average_evaluator(
                times=times,
                qualities=self.qualities,
                num=self.num_variations,
                stddev=self.variations_stddev)
        else:
            self.evaluator = PortfolioEvaluator(times=times, qualities=self.qualities)

        # the underlying schedule represented by its configs and max runtimes
        self.schedule_config_ids = []
        self.schedule_runtimes = []

    def _retrieve_information(self):
        """Parse the passed report."""
        solved_problems_per_domain = defaultdict(set)
        commandline_configs_map = {}
        data = defaultdict(dict)
        for (domain, problem, config), run in self.runs.items():
            quality = run.get('quality')
            coverage = run.get('coverage')
            if coverage:
                total_time = run.get('total_time')
                cost = run.get('cost')
                # solved problems shouldn't miss these attributes otherwise
                # unsolved problems have to be masked differently
                assert total_time is not None
                # add problem only if it has been solved
                solved_problems_per_domain[domain].add(problem)
            else: # mask unsolved problems missing
                total_time = None
                cost = None
            # Backwards compatibility.
            commandline_config = run.get('component_options') or run.get('commandline_config')
            commandline_configs_map[config] = commandline_config
            data[(domain, problem)][config] = (total_time, cost, quality)

        for domain, problem in self.problems:
            self.total_times.append([])
            self.costs.append([])
            self.qualities.append([])
            self.trained.append([])
            num_problems = len(solved_problems_per_domain[domain])
            for config in self.configs:
                total_time, cost, quality = data[(domain, problem)][config]
                self.total_times[-1].append(total_time)
                self.costs[-1].append(cost)
                if self.absolute_quality:
                    self.qualities[-1].append(quality)
                else:
                    # normalize each quality by the number of solved tasks
                    if num_problems == 0:
                        normalized_quality = 0
                    else:
                        normalized_quality = float(quality) / num_problems
                    self.qualities[-1].append(normalized_quality)
                # True if the config was trained on the domain otherwise False
                self.trained[-1].append(same_domain(domain, config))
        for config in self.configs:
            self.commandline_configs.append(self._format_commandline_config(
                commandline_configs_map[config]))

    def _format_commandline_config(self, commandline_config):
        commandline_config = [i.strip() for i in commandline_config]
        # Don't use bounds for optimal planning.
        if self.optimal:
            return commandline_config

        for i, entry in enumerate(commandline_config):
            if entry == "--search":
                commandline_config[i+1] = (commandline_config[i+1][:-1] +
                                           ",bound=BOUND)")
        return commandline_config

    def get_domain_score(self, domain, runtimes):
        problem_numbers = self.domain_to_problem_indices[domain]
        return self.evaluator.score(runtimes, problem_numbers)

    def get_max_domain_score(self, domain):
        return self.get_domain_score(domain, numpy.infty)

    def schedule(self):
        """The portfolio's schedule is always created on-the-fly"""
        return [(config_id, runtime) for runtime, config_id in
                zip(self.schedule_runtimes, self.schedule_config_ids)
                if runtime > 0]

    def sorted_runtimes(self):
        """
        Return the runtimes in the order of their configs in self.configs. The
        runtime is 0 for configs not included in the schedule.
        """
        configs_to_times = dict(zip(self.schedule_config_ids,
                                    self.schedule_runtimes))
        return (numpy.array([configs_to_times.get(config_id, 0) for config_id in
                range(len(self.configs))]))

    def reduce_score_based(self, runtimes, granularity=1):
        """
        Reduces the runtime for each config as long as the resulting evaluation
        score does not decrease.
        """
        score = self.evaluator.score(runtimes)
        for id_ in self.schedule_config_ids:
            while runtimes[id_] > granularity + EPSILON:
                runtimes[id_] -= granularity
                if self.evaluator.score(runtimes) < score:
                    runtimes[id_] += granularity
                    break

    def print_portfolio(self):
        """Print the generated portfolio."""
        domain_quotas = []
        for domain_id, domain in enumerate(self.domains.keys()):
            domain_score = self.get_domain_score(domain, self.sorted_runtimes())
            max_domain_score = self.get_max_domain_score(domain)
            if max_domain_score == 0:
                domain_quotas.append(0)
            else:
                domain_quotas.append(float(domain_score) / max_domain_score)
        domain_quotas = numpy.array(domain_quotas)

        rows = []
        rows.append("#! /usr/bin/env python")
        rows.append("# -*- coding: utf-8 -*-\n")
        rows.append("import portfolio\n")

        rows.append('"""')
        rows.append("Portfolio generator: %s\n" % self.portfolio_name)
        rows.append("Score: %.4f" % self.evaluator.score(
            self.sorted_runtimes()))
        rows.append('Average score quota: %.2f' %
                    numpy.mean(domain_quotas, dtype=numpy.float64))
        rows.append('Standard deviation of score quota: %.2f' %
                    numpy.std(domain_quotas, dtype=numpy.float64))
        rows.append('Training set: %s' % self.eval_dir.split("/")[-1])
        rows.append("\nSettings:")
        for setting in self.settings:
            rows.append("   %s" % setting)
        rows.append('"""\n')

        rows.append("CONFIGS = [")
        params = []
        schedule = self.schedule()
        for config_id, runtime in schedule:
            config = self.configs[config_id]
            cmdline_config = self.commandline_configs[config_id]
            params.append("    # " + config)
            params.append("    (" + str(int(runtime)) + ", " +
                          str(cmdline_config) + "), ")
        rows.append("\n".join(params)[:-2])
        rows.append("]\n")

        if not self.optimal and self.final_config:
            rows.append("FINAL_CONFIG = [%s]\n" % self.final_config)

        # TODO: Add final config if it is set
        rows.append("portfolio.run(configs=CONFIGS, optimal=%s, timeout=%d)" %
                    (self.optimal, self.plantime))

        with open(self.portfolio_file, "w") as file_handler:
            file_handler.write("\n".join(rows))
        # Make file executable
        os.chmod(self.portfolio_file, 0755)

    def parse_portfolio(self, filename):
        """ Parse portfolio. returns runtimes list.
        Fails if portfolio was not generated on the same eval directory
        """
        def parse_array(fd):
            open_brakets = 1
            config_names = []
            configs_str = '['
            for line in fd:
                open_brakets += line.count('[')
                open_brakets -= line.count(']')
                configs_str += line
                line = line.strip()
                if line.startswith('#'):
                    config_names.append(line[1:].strip())
                if not open_brakets:
                        # end of list
                        try:
                            configs = eval(configs_str)
                        except SyntaxError:
                            raise InvalidPortfolio()
                        return config_names, configs

        configs = []
        config_names = []
        with open(filename) as fd:
            for line in fd:
                if line.startswith('CONFIGS = ['):
                    names, conf = parse_array(fd)
                    config_names += names
                    configs += conf
                elif line.startswith('FINAL_CONFIG = ['):
                    names, conf = parse_array(fd)
                    config_names += names

        if not configs:
            logging.error('Could not parse portfolio %s' % filename)
            raise InvalidPortfolio()
        times = [time for time, _ in configs]
        # append final time
        times.append(self.plantime - sum(times))
        missing_configs = set(config_names) - set(self.configs)
        if missing_configs:
            logging.error(
                "The following configs appear in portfolio but "
                "NOT in eval directory. Portfolio is INVALID:"
                "\n%s" % missing_configs)
            raise InvalidPortfolio()

        configs_to_times = dict(zip(config_names, times))
        runtimes = [configs_to_times.get(conf, 0) for conf in self.configs]
        logging.info("parsed runtimes from %s are: %s" %
                     (filename.split("/")[-1], runtimes))
        logging.info("total runtime is %d" % sum(runtimes))
        return numpy.array(runtimes)

    def get_markup(self):
        sorted_runtimes = self.sorted_runtimes()
        total_runtime = sum(sorted_runtimes)

        rows = []
        rows.append('= %s =' % self.portfolio_name)
        rows.append('%s' % self.report_descr)
        for setting in self.settings:
            rows.append('- %s' % setting)
        rows.append('')
        rows.append('')

        rows.append('== Results ==')
        rows.append('The results on the training set "%s".' % self.eval_dir)
        rows.append('|| Score |')
        rows.append('| %.2f / %.2f |' % (self.evaluator.score(sorted_runtimes),
                                         self.evaluator.max_score()))

        rows.append('== Schedule ==')
        rows.append('|| Id | Config | Runtime | Score | Coverage |')
        solved_problems = ((self.total_times < (sorted_runtimes + EPSILON)) *
                           (numpy.not_equal(self.total_times, None)))
        num_solved_by_config = numpy.sum(solved_problems, axis=0)
        for config_id, runtime in zip(self.schedule_config_ids,
                                      self.schedule_runtimes):
            rows.append('| %i | %s | %i | %.2f | %i |' % (config_id,
                self.configs[config_id], runtime,
                self.evaluator.configs_scores(sorted_runtimes)[config_id],
                num_solved_by_config[config_id]))
        rows.append('|  | TOTAL | %i |  | %i |' % (total_runtime,
                numpy.sum(numpy.max(solved_problems, axis=1))))

        rows.append('== Domains ==')
        rows.append('|| Id | Domain | Score |')
        domain_quotas = []
        for domain_id, domain in enumerate(self.domains.keys()):
            domain_score = self.get_domain_score(domain, sorted_runtimes)
            max_domain_score = self.get_max_domain_score(domain)
            if max_domain_score == 0:
                domain_quotas.append(0)
            else:
                domain_quotas.append(float(domain_score) / max_domain_score)
            rows.append('| %i | %s | %.2f / %.2f | ' %
                        (domain_id, domain, domain_score, max_domain_score))
        domain_quotas = numpy.array(domain_quotas)
        rows.append('**Average score quota**: %.2f\n' %
                    numpy.mean(domain_quotas, dtype=numpy.float64))
        rows.append('**Standard deviation of score quota**: %.2f' %
                    numpy.std(domain_quotas, dtype=numpy.float64))

        rows.append('== Training data ==')
        rows.append('General training data information.')
        rows.append('|| Number of Problems | Number of Solved Problems |')
        solved_problems = numpy.not_equal(self.total_times, None)
        solved_by_any_config = numpy.sum(solved_problems, axis=1) > 0
        num_solved_in_training_data = numpy.sum(solved_by_any_config)
        rows.append('| %i | %i |' % (len(self.problems),
                                     num_solved_in_training_data))

        rows.append('=== List of Problems ===')
        rows.append('The bolded qualities and times indicate problems that can '
                    'be solved by this portfolio in the training set.')
        rows.append('|| Domain | Problem | %s |' %
                    ' | '.join(str(i) for i in range(len(self.configs))))
        for problem_id, (domain, problem) in enumerate(self.problems):
            columns = []
            columns.append(domain)
            columns.append(problem)
            for config_id, config in enumerate(self.configs):
                total_time = self.total_times[problem_id][config_id]
                if total_time is None:
                    columns.append("")
                else:
                    runtime = sorted_runtimes[config_id]
                    if total_time <= runtime:
                        columns.append("**%.2f**" % (total_time))
                    else:
                        columns.append("%.2f" % (total_time))
            rows.append('| %s |' % ' | '.join(columns))

        return '\n'.join(rows)


class PortfolioEvaluator(object):
    """
    Calculates the performance of an portfolio on a given benchmark. The
    benchmark is given as the an array of times and qualities of all configs on
    all problems.
    """
    def __init__(self, times, qualities):
        """
        times, qualities: numpy 2d arrays of equal size
        """
        self.times = times
        self.qualities = qualities

    def score(self, runtimes, problems_id_list=None):
        """
        Returns score for given config runtimes. Runtimes for all configs must
        be present. Configs that are not used within the portfolio should have a
        runtime of 0.
        """
        runtimes = numpy.array(runtimes)
        if problems_id_list is None:
            # Select all problems
            problems_id_list = slice(None)
        solved_problems_quality = numpy.where(
            (self.times[problems_id_list, :] < (runtimes + EPSILON)),
            self.qualities[problems_id_list, :],
            0)
        best_quality_per_config = numpy.max(solved_problems_quality, axis=1)
        return numpy.sum(best_quality_per_config)

    def max_score(self, problems_id_list=None):
        return self.score(numpy.infty)

    def configs_scores(self, runtimes, problems_id_list=None):
        runtimes = numpy.array(runtimes)
        if problems_id_list is None:
            # Select all problems
            problems_id_list = slice(None)
        solved_problems_quality = numpy.where(
            (self.times[problems_id_list, :] < (runtimes + EPSILON)),
            self.qualities[problems_id_list, :],
            0)
        return numpy.sum(solved_problems_quality, axis=0)


class PortfolioAverageEvaluator(object):
    """
    Evaluates average score on a variation of times.
    """
    def __init__(self, times_variations, qualities):
        """ times_variations has to be an iterable over times arrays. """
        # create evaluators for time variations
        self.evaluators = [PortfolioEvaluator(t, qualities)
                           for t in times_variations]

    def max_score(self, problems_id_list=None):
        return self.score(numpy.infty)

    def score(self, runtimes, problems_id_list=None):
        """ Average score on all variations. """
        return (sum(e.score(runtimes, problems_id_list)
                for e in self.evaluators) / len(self.evaluators))

    def configs_scores(self, runtimes, problems_id_list=None):
        configs_scores_vars = numpy.array([e.configs_scores(
            runtimes, problems_id_list) for e in self.evaluators])
        return numpy.sum(configs_scores_vars, axis=0) / len(self.evaluators)


def norm_time_variation_generator(times, stddev=1.0, num=100):
    """
    Genrates 'num' normally distributed variations of total_time. Times have to
    be pure numerical. You are responsible for replacing missing values with
    appropriate numerical ones.
    """
    return (_times_variation(times, stddev=stddev) for _ in range(num))


def _times_variation(times, stddev=1.0):
    """ Returns times with normally distributed noise added. """
    var_time = times + numpy.random.normal(scale=stddev, size=times.shape)
    return numpy.round(numpy.maximum(var_time, 0))


def get_norm_average_evaluator(times, qualities, num=100, stddev=1):
    """
    Factory method for PortfolioAverageEvaluator with normal distributed
    variations.
    """
    variations = norm_time_variation_generator(times, stddev=stddev, num=num)
    return PortfolioAverageEvaluator(variations, qualities)
