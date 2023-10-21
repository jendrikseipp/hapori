import logging
import random

import numpy

from portfolio import Portfolio, EPSILON, InvalidPortfolio


class RanitSearchPortfolio(Portfolio):
    def __init__(self, *args, **kwargs):
        Portfolio.__init__(self, *args, **kwargs)
        self.portfolios = kwargs.get('portfolios', [])
        self.max_tries = kwargs.get('max_tries', 20000)
        self.use_reduce = kwargs.get('use_reduce', False)

        self.settings.append('Maximum number of tries: %i' % self.max_tries)
        portfolios = []
        if self.portfolios:
            for portfolio in self.portfolios:
                portfolios.append(portfolio.split("/")[-1])
        else:
            portfolios.append('Stupid uniform')
        self.settings.append('Used portfolios: {%s}' % ",".join(portfolios))

        self.portfolio_name = 'Fastr Ranitbest Portfolio'
        self.report_descr = (('A portfolio of **%i seconds** for'
                              'the FD planning '
                              'algorithm. Generation based on the successive '
                              'improvement of a set of given portfolio '
                              'schedules.') % self.plantime, )

    def compute_portfolio(self):
        """ implementation of compute_portfolio method as it is implemented in
        IncreasingTimeslotPortfolio.
        """
        self.schedule_runtimes = numpy.zeros(len(self.configs))
        self.schedule_config_ids = range(len(self.configs))
        best_score = -1
        best_runtimes = None
        if self.portfolios:
            for filename in self.portfolios:
                logging.info(
                    "using portfolio %s as initialization" % filename)
                try:
                    runtimes = self.parse_portfolio(filename)
                except InvalidPortfolio:
                    logging.info("Skipping this file")
                else:
                    score, runtimes = self.improve_portfolio(runtimes)
                    if score > best_score:
                        logging.info(
                            'The portfolio derived from %s is the new best. '
                            'Score: %s' % (filename, score))
                        best_score = score
                        best_runtimes = runtimes
        else:
            logging.warn("No initial portfolios given, using stupid uniform "
              "initialization")
            num_configs = len(self.configs)
            runtimes = numpy.ones(num_configs,
                          dtype=int) * self.plantime / num_configs
            score, best_runtimes = self.improve_portfolio(runtimes)
        logging.info("DONE computing portfolio, final score: %s" % best_score)
        self.schedule_runtimes = best_runtimes

    def successors(self, runtimes):
        num_configs = len(runtimes)
        max_runtime = self.plantime / 2

        def rand_swap(succ_runtimes):
            """ swap a random runtime proportion between to random configs ==
            """
            while True:
                # sample to ids without replacement
                id1, id2 = random.sample(xrange(num_configs), 2)
                # choose a random granularity
                # would it make sense to sample from a non-unifrom distribution
                # eg prefering small values?
                delta = int(random.random() * max_runtime)
                if succ_runtimes[id1] < delta:
                    # doent work out this way, continue
                    continue

                succ_runtimes[id1] -= delta
                succ_runtimes[id2] += self.plantime - numpy.sum(succ_runtimes)
                return succ_runtimes

        def rand_all(succ_runtimes):
            """ takes a small time proportion from all configs
            and assign it to a random config
            """
            delta = int(random.random() * self.plantime)
            num_active = numpy.sum(
                numpy.where(succ_runtimes > EPSILON))
            single_delta = int(delta / num_active)
            profiteer = random.randint(0, num_configs - 1)

            total_loosers = succ_runtimes <= single_delta
            total_loosers[profiteer] = False
            # sum of runtime of configs that will be totally disabled
            # total_loss = numpy.sum(succ_runtimes[total_loosers])
            # set their runtime to zero
            succ_runtimes[total_loosers] = 0
            # loosers of single_delta
            loosers = numpy.invert(total_loosers)
            loosers[profiteer] = False
            succ_runtimes[loosers] -= single_delta
            # total_loss += numpy.sum(loosers) * single_delta
            succ_runtimes[profiteer] += (self.plantime -
                numpy.sum(succ_runtimes))
            return succ_runtimes
        while True:
            # swap random
            yield rand_swap(runtimes.copy())
            # take from all random
            yield rand_all(runtimes.copy())

    def improve_portfolio(self, runtimes):
        """Tries to improve the given schedule/portfolio.

        Returns (quality, improved_runtimes).
        """
        logging.info("improving runtimes...")
        best_runtimes = runtimes
        best_score = self.evaluator.score(runtimes)
        logging.info("initialized with portfolio with score %s" % best_score)
        while True:
            tries = 0
            #reduced = False
            for succ_runtimes in self.successors(best_runtimes):
                tries += 1
                succ_score = self.evaluator.score(succ_runtimes)
                if succ_score > best_score:
                    logging.info("found a better portfolio after %d tries, "
                                 "score: %s" % (tries, succ_score))
                    logging.info("runtimes: %s" % succ_runtimes)
                    # logging.info("total time: %s" % numpy.sum(succ_runtimes))
                    logging.info(
                        "runtime delta %s" % (succ_runtimes - best_runtimes))
                    best_score = succ_score
                    best_runtimes = succ_runtimes
                    if self.use_reduce:
                        logging.info("reducing...")
                        self.reduce_score_based(best_runtimes)
                    # continue using new best runntimes
                    break
                    # todo: implement some timeout, random restart etc
                if not tries % 1000:
                    logging.info("Try %d, trying harder..." % tries)
                #if not tries % 500 and not reduced:

                 #   reduced = True
                if tries > self.max_tries:
                    logging.info("reached max_tries, giving up improving")
                    return best_score, best_runtimes
