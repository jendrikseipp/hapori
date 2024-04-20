from collections import defaultdict
from enum import Enum, auto
import math
import time

import numpy as np
from downward.reports import PlanningReport

TIMEOUT = 1800
EPSILON = 0.0001


def normalize_domain_name(name):
    removals = ["-", "_"]
    removals += ["strips", "adl", "opt08", "sat08"]
    for item in removals:
        name = name.replace(item, "")
    return name


def same_domain(name1, name2):
    return normalize_domain_name(name1) == normalize_domain_name(name2)


class Track(Enum):
    OPT = auto()
    SAT = auto()
    AGL = auto()


class Portfolio(PlanningReport):
    def __init__(self, track, absolute_score=True, plantime=TIMEOUT, **kwargs):
        self.track = track
        self.plantime = plantime
        PlanningReport.__init__(self, **kwargs)
        # Use absolute scores instead of scores normalized by the number
        # of problems per domain.
        self.absolute_score = absolute_score
        # Evaluates performance on a set of "num" training variations
        self.num_variations = None
        # Standard derivation of variations of runtime
        self.variations_stddev = kwargs.get("variations-stddev", 30)

        # for output information - this can be overwritten by subclasses
        self.portfolio_name = type(self).__name__

        # for the portfolio file - additional information can be added in the
        # subclasses
        self.settings = []
        self.settings.append("Maximum plantime: %i" % self.plantime)
        self.settings.append("Absolute score: %s" % self.absolute_score)
        if self.num_variations:
            self.settings.append("Number of variations: %i" % self.num_variations)
            self.settings.append(
                "Standard deviation of the variations: %f" % self.variations_stddev
            )

    def compute_portfolio(self):
        raise NotImplementedError

    def write(self):
        if self.outfile.endswith(".html"):
            self.portfolio_file = self.outfile.replace(".html", ".py")
        else:
            self.portfolio_file = self.outfile + ".html"
        start_time = time.process_time()
        self.compute_portfolio()
        end_time = time.process_time()
        self.time_for_computing_portfolio = end_time - start_time
        PlanningReport.write(self)
        self.print_portfolio()

    def _scan_data(self):
        PlanningReport._scan_data(self)

        self.runtimes = []  # runtime for each problem-config pair
        self.scores = []  # score (coverage, quality or agile score) for each problem-config pair

        self._retrieve_information()

        # Save a mapping from domain names to indices in self.problems
        self.domain_to_problem_indices = defaultdict(list)
        for prob_id, (domain, _problem) in enumerate(sorted(self.problem_runs.keys())):
            self.domain_to_problem_indices[domain].append(prob_id)

        # transform data matrices to np matrices for easier handling
        self.runtimes = np.array(self.runtimes)
        self.scores = np.array(self.scores)

        # replace missing times with infty for the evaluator
        times = np.where(np.equal(self.runtimes, None), np.inf, self.runtimes).astype(
            "float"
        )
        # define the evaluator
        if self.num_variations:
            self.evaluator = get_norm_average_evaluator(
                times=times,
                scores=self.scores,
                num=self.num_variations,
                stddev=self.variations_stddev,
            )
        else:
            self.evaluator = PortfolioEvaluator(times=times, scores=self.scores)

        # the underlying schedule represented by its configs and max runtimes
        self.schedule_config_ids = []
        self.schedule_runtimes = []

    def _retrieve_information(self):
        """Parse the passed report."""
        best_costs = {}
        for (domain, problem, _algo), run in self.runs.items():
            if run["coverage"]:
                cost = run["cost"]
                assert cost is not None
                if (domain, problem) not in best_costs:
                    best_costs[(domain, problem)] = cost
                else:
                    best_costs[(domain, problem)] = min(
                        best_costs[(domain, problem)], cost
                    )

        solved_problems_per_domain = defaultdict(set)
        data = defaultdict(dict)
        for (domain, problem, config), run in self.runs.items():
            score = 0.0
            runtime = run.get("cpu_time")
            if runtime is not None:
                if self.track == Track.AGL:
                    if runtime <= 1.0:
                        score = 1.0
                    else:
                        score = 1.0 - (math.log10(runtime) / math.log10(1800))
                elif self.track == Track.SAT:
                    quality = run.get("quality")
                    if quality is None:
                        cost = run["cost"]
                        best_cost = best_costs[(domain, problem)]
                        assert best_cost <= cost, (best_cost, run)
                        if cost == 0:
                            assert best_cost == 0
                            score = 1.0
                        else:
                            score = float(best_cost) / cost
                    else:
                        score = run["quality"]
                elif self.track == Track.OPT:
                    cost = run["cost"]
                    assert cost == best_costs[(domain, problem)], (problem, config)
                    score = 1.0
                else:
                    assert False, self.track

            coverage = run.get("coverage")
            if coverage:
                runtime = run.get("cpu_time")
                cost = run.get("cost")
                # solved problems shouldn't miss these attributes otherwise
                # unsolved problems have to be masked differently
                assert runtime is not None, run
                # add problem only if it has been solved
                solved_problems_per_domain[domain].add(problem)
            else:  # mask unsolved problems missing
                runtime = None
                cost = None
            data[(domain, problem)][config] = (runtime, cost, score)
            #print(domain, problem, config, runtime, cost, score)

        for domain, problem in sorted(self.problem_runs.keys()):
            self.runtimes.append([])
            self.scores.append([])
            num_problems = len(solved_problems_per_domain[domain])
            for config in self.algorithms:
                runtime, cost, score = data[(domain, problem)][config]
                self.runtimes[-1].append(runtime)
                if self.absolute_score:
                    self.scores[-1].append(score)
                else:
                    # normalize each score by the number of solved tasks
                    if num_problems == 0:
                        normalized_score = 0
                    else:
                        normalized_score = float(score) / num_problems
                    self.scores[-1].append(normalized_score)

    def get_domain_score(self, domain, runtimes):
        problem_numbers = self.domain_to_problem_indices[domain]
        return self.evaluator.score(runtimes, problem_numbers)

    def get_max_domain_score(self, domain):
        return self.get_domain_score(domain, np.infty)

    def schedule(self):
        """The portfolio's schedule is always created on-the-fly."""
        return [
            (config_id, runtime)
            for runtime, config_id in zip(
                self.schedule_runtimes, self.schedule_config_ids
            )
            if runtime > 0
        ]

    def sorted_runtimes(self):
        """Return the runtimes in the order of their configs in self.algorithms. The
        runtime is 0 for configs not included in the schedule.
        """
        configs_to_times = dict(
            list(zip(self.schedule_config_ids, self.schedule_runtimes))
        )
        return np.array(
            [
                configs_to_times.get(config_id, 0)
                for config_id in range(len(self.algorithms))
            ]
        )

    def reduce_score_based(self, runtimes, granularity=1):
        """Reduces the runtime for each config as long as the resulting evaluation
        score does not decrease.
        """
        score = self.evaluator.score(runtimes)
        for id_ in self.schedule_config_ids:
            while runtimes[id_] > granularity + EPSILON:
                runtimes[id_] -= granularity
                if self.evaluator.score(runtimes) < score:
                    runtimes[id_] += granularity
                    break

    def get_mapping_from_algorithm_to_planner_config(self):
        mapping = {}
        for run in self.runs.values():
            mapping[run["algorithm"]] = (run["planner"], run["config"])
        return mapping

    def print_portfolio(self):
        """Print the generated portfolio."""
        domain_quotas = []
        for domain in self.domains:
            domain_score = self.get_domain_score(domain, self.sorted_runtimes())
            max_domain_score = self.get_max_domain_score(domain)
            if max_domain_score == 0:
                domain_quotas.append(0)
            else:
                domain_quotas.append(float(domain_score) / max_domain_score)
        domain_quotas = np.array(domain_quotas)

        rows = []
        rows.append('"""')
        rows.append("Portfolio generator: %s\n" % self.portfolio_name)
        rows.append(f"Time for computing portfolio: {self.time_for_computing_portfolio:.2f}s")
        rows.append(f"Score: {self.evaluator.score(self.sorted_runtimes()):.2f}")
        rows.append(
            "Average score quota: %.2f" % np.mean(domain_quotas, dtype=np.float64)
        )
        rows.append(
            "Standard deviation of score quota: %.2f"
            % np.std(domain_quotas, dtype=np.float64)
        )
        rows.append("Training set: %s" % self.eval_dir.split("/")[-1])
        rows.append("\nSettings:")
        for setting in self.settings:
            rows.append("   %s" % setting)
        rows.append('"""\n')

        planner_configs = self.get_mapping_from_algorithm_to_planner_config()
        rows.append("PLANNERS = [")
        params = []
        schedule = self.schedule()
        for algo_id, runtime in schedule:
            if runtime == 0:
                continue
            algo = self.algorithms[algo_id]
            planner, config = planner_configs[algo]
            params.append("    # " + algo)
            params.append(f"    ({int(runtime)}, ['{planner}', '{config}']),")
        rows.append("\n".join(params))
        rows.append("]\n")

        with open(self.portfolio_file, "w") as f:
            f.write("\n".join(rows))

    def get_markup(self):
        sorted_time_limits = self.sorted_runtimes()
        total_runtime = sum(sorted_time_limits)

        rows = []
        rows.append("= %s =" % self.portfolio_name)
        rows.extend("- %s" % setting for setting in self.settings)
        rows.append("")
        rows.append("")

        rows.append("== Results ==")
        rows.append('The results on the training set "%s".' % self.eval_dir)
        rows.append("|| Score |")
        rows.append(
            f"| {self.evaluator.score(sorted_time_limits):.2f} / {self.evaluator.max_score():.2f} |"
        )

        rows.append("== Schedule ==")
        rows.append("|| Id | Config | Time Limit | Score | Coverage |")
        float_runtimes = np.where(self.runtimes == None, np.inf, self.runtimes)
        solved_problems = float_runtimes < (sorted_time_limits + EPSILON)
        num_solved_by_config = np.sum(solved_problems, axis=0)
        for config_id, runtime in zip(self.schedule_config_ids, self.schedule_runtimes):
            rows.append(
                "| %i | %s | %i | %.2f | %i |"
                % (
                    config_id,
                    self.algorithms[config_id],
                    runtime,
                    self.evaluator.configs_scores(sorted_time_limits)[config_id],
                    num_solved_by_config[config_id],
                )
            )
        rows.append(
            "|  | TOTAL | %i |  | %i |"
            % (total_runtime, np.sum(np.max(solved_problems, axis=1)))
        )

        rows.append("== Domains ==")
        rows.append("|| Id | Domain | Score |")
        domain_quotas = []
        for domain_id, domain in enumerate(self.domains.keys()):
            domain_score = self.get_domain_score(domain, sorted_time_limits)
            max_domain_score = self.get_max_domain_score(domain)
            if max_domain_score == 0:
                domain_quotas.append(0)
            else:
                domain_quotas.append(float(domain_score) / max_domain_score)
            rows.append(
                "| %i | %s | %.2f / %.2f | "
                % (domain_id, domain, domain_score, max_domain_score)
            )
        domain_quotas = np.array(domain_quotas)
        rows.append(
            "**Average score quota**: %.2f\n" % np.mean(domain_quotas, dtype=np.float64)
        )
        rows.append(
            "**Standard deviation of score quota**: %.2f"
            % np.std(domain_quotas, dtype=np.float64)
        )

        rows.append("== Training data ==")
        rows.append("General training data information.")
        rows.append("|| Number of Problems | Number of Solved Problems |")
        solved_problems = np.not_equal(self.runtimes, None)
        solved_by_any_config = np.sum(solved_problems, axis=1) > 0
        num_solved_in_training_data = np.sum(solved_by_any_config)
        rows.append(
            "| %i | %i |" % (len(self.problem_runs), num_solved_in_training_data)
        )

        rows.append("=== List of Problems ===")
        rows.append(
            "The bolded scores and times indicate problems that can "
            "be solved by this portfolio in the training set."
        )
        rows.append(
            "|| Domain | Problem | %s |"
            % " | ".join(str(i) for i in range(len(self.algorithms)))
        )
        for problem_id, (domain, problem) in enumerate(
            sorted(self.problem_runs.keys())
        ):
            columns = []
            columns.append(domain)
            columns.append(problem)
            for config_id in range(len(self.algorithms)):
                runtime = self.runtimes[problem_id][config_id]
                if runtime is None:
                    columns.append("")
                else:
                    time_limit = sorted_time_limits[config_id]
                    if runtime <= time_limit:
                        columns.append("**%.2f**" % (runtime))
                    else:
                        columns.append("%.2f" % (runtime))
            rows.append("| %s |" % " | ".join(columns))

        return "\n".join(rows)


class PortfolioEvaluator:
    """Calculates the performance of an portfolio on a given benchmark. The
    benchmark is given as the an array of times and scores of all configs on
    all problems.
    """

    def __init__(self, times, scores):
        """times, scores: np 2d arrays of equal size."""
        self.times = times
        self.scores = scores

    def score(self, runtimes, problems_id_list=None):
        """Returns score for given config runtimes. Runtimes for all configs must
        be present. Configs that are not used within the portfolio should have a
        runtime of 0.
        """
        runtimes = np.array(runtimes)
        if problems_id_list is None:
            # Select all problems
            problems_id_list = slice(None)
        solved_problems_score = np.where(
            (self.times[problems_id_list, :] < (runtimes + EPSILON)),
            self.scores[problems_id_list, :],
            0,
        )
        best_score_per_config = np.max(solved_problems_score, axis=1)
        return np.sum(best_score_per_config)

    def max_score(self):
        return self.score(np.infty)

    def configs_scores(self, runtimes, problems_id_list=None):
        runtimes = np.array(runtimes)
        if problems_id_list is None:
            # Select all problems
            problems_id_list = slice(None)
        solved_problems_score = np.where(
            (self.times[problems_id_list, :] < (runtimes + EPSILON)),
            self.scores[problems_id_list, :],
            0,
        )
        return np.sum(solved_problems_score, axis=0)


class PortfolioAverageEvaluator:
    """Evaluates average score on a variation of times."""

    def __init__(self, times_variations, scores):
        """times_variations has to be an iterable over times arrays."""
        # create evaluators for time variations
        self.evaluators = [PortfolioEvaluator(t, scores) for t in times_variations]

    def max_score(self):
        return self.score(np.infty)

    def score(self, runtimes, problems_id_list=None):
        """Average score on all variations."""
        return sum(e.score(runtimes, problems_id_list) for e in self.evaluators) / len(
            self.evaluators
        )

    def configs_scores(self, runtimes, problems_id_list=None):
        configs_scores_vars = np.array(
            [e.configs_scores(runtimes, problems_id_list) for e in self.evaluators]
        )
        return np.sum(configs_scores_vars, axis=0) / len(self.evaluators)


def norm_time_variation_generator(times, stddev=1.0, num=100):
    """Genrates 'num' normally distributed variations of runtime. Times have to
    be pure numerical. You are responsible for replacing missing values with
    appropriate numerical ones.
    """
    return (_times_variation(times, stddev=stddev) for _ in range(num))


def _times_variation(times, stddev=1.0):
    """Returns times with normally distributed noise added."""
    var_time = times + np.random.normal(scale=stddev, size=times.shape)
    return np.round(np.maximum(var_time, 0))


def get_norm_average_evaluator(times, scores, num=100, stddev=1):
    """Factory method for PortfolioAverageEvaluator with normal distributed
    variations.
    """
    variations = norm_time_variation_generator(times, stddev=stddev, num=num)
    return PortfolioAverageEvaluator(variations, scores)
