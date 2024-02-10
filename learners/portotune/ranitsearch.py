import logging
import random

import numpy as np

from portfolio import EPSILON, Portfolio


class RanitSearchPortfolio(Portfolio):
    def __init__(self, random_seed, *args, **kwargs):
        Portfolio.__init__(self, *args, **kwargs)
        self.random_seed = random_seed
        self.settings.append(f"Random seed: {self.random_seed}")
        self.max_tries = 20000
        self.use_reduce = False

    def compute_portfolio(self):
        """Implementation of compute_portfolio method as it is implemented in
        IncreasingTimeslotPortfolio.
        """
        random.seed(self.random_seed)
        self.schedule_runtimes = np.zeros(len(self.algorithms))
        self.schedule_config_ids = list(range(len(self.algorithms)))
        num_configs = len(self.algorithms)
        runtimes = np.ones(num_configs, dtype=int) * self.plantime / num_configs
        score, best_runtimes = self.improve_portfolio(runtimes)
        logging.info("DONE computing portfolio, final score: %s" % score)
        self.schedule_runtimes = best_runtimes

    def successors(self, runtimes):
        num_configs = len(runtimes)
        max_runtime = self.plantime / 2

        def rand_swap(succ_runtimes):
            """Swap a random runtime proportion between to random configs ==."""
            while True:
                # sample to ids without replacement
                id1, id2 = random.sample(range(num_configs), 2)
                # choose a random granularity
                # would it make sense to sample from a non-unifrom distribution
                # eg prefering small values?
                delta = int(random.random() * max_runtime)
                if succ_runtimes[id1] < delta:
                    # doent work out this way, continue
                    continue

                succ_runtimes[id1] -= delta
                succ_runtimes[id2] += self.plantime - np.sum(succ_runtimes)
                return succ_runtimes

        def rand_all(succ_runtimes):
            """Takes a small time proportion from all configs
            and assign it to a random config.
            """
            delta = int(random.random() * self.plantime)
            num_active = np.sum(np.where(succ_runtimes > EPSILON))
            single_delta = int(delta / num_active)
            profiteer = random.randint(0, num_configs - 1)

            total_loosers = succ_runtimes <= single_delta
            total_loosers[profiteer] = False
            # sum of runtime of configs that will be totally disabled
            # total_loss = numpy.sum(succ_runtimes[total_loosers])
            # set their runtime to zero
            succ_runtimes[total_loosers] = 0
            # loosers of single_delta
            loosers = np.invert(total_loosers)
            loosers[profiteer] = False
            succ_runtimes[loosers] -= single_delta
            # total_loss += numpy.sum(loosers) * single_delta
            succ_runtimes[profiteer] += self.plantime - np.sum(succ_runtimes)
            return succ_runtimes

        while True:
            # swap random
            yield rand_swap(runtimes.copy())
            # take from all random
            yield rand_all(runtimes.copy())

    def improve_portfolio(self, runtimes):
        """Tries to improve the given schedule/portfolio.

        Returns (score, improved_runtimes).
        """
        logging.info("improving runtimes...")
        best_runtimes = runtimes
        best_score = self.evaluator.score(runtimes)
        logging.info("initialized with portfolio with score %s" % best_score)
        while True:
            tries = 0
            # reduced = False
            for succ_runtimes in self.successors(best_runtimes):
                tries += 1
                succ_score = self.evaluator.score(succ_runtimes)
                if succ_score > best_score:
                    logging.info(
                        "found a better portfolio after %d tries, "
                        "score: %s" % (tries, succ_score)
                    )
                    logging.info("runtimes: %s" % succ_runtimes)
                    # logging.info("total time: %s" % numpy.sum(succ_runtimes))
                    logging.info("runtime delta %s" % (succ_runtimes - best_runtimes))
                    best_score = succ_score
                    best_runtimes = succ_runtimes
                    if self.use_reduce:
                        logging.info("reducing...")
                        self.reduce_score_based(best_runtimes)
                    # continue using new best runntimes
                    break
                    # TODO: implement some timeout, random restart etc
                if not tries % 1000:
                    logging.info("Try %d, trying harder..." % tries)
                # if not tries % 500 and not reduced:

                #   reduced = True
                if tries > self.max_tries:
                    logging.info("reached max_tries, giving up improving")
                    return best_score, best_runtimes
