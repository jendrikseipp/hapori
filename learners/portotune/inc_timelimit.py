import numpy as np

from portfolio import EPSILON, Portfolio


class IncreasingTimelimitPortfolio(Portfolio):
    def __init__(self, stepsize, *args, **kwargs):
        Portfolio.__init__(self, *args, **kwargs)
        self.stepsize = stepsize

        self.settings.append("Timeslot size: %i" % self.stepsize)

        self.portfolio_name = "Increasing Timeslots Portfolio"
        self.report_descr = (
            "A portfolio of **%i seconds**. Generation based on "
            "increasing timeslots of **%i seconds**."
        ) % (self.plantime, self.stepsize)

    def compute_portfolio(self):
        schedule_runtimes = []
        schedule_configs = []
        used_configs = {}
        overall_runtime = 0
        current_timeslot = self.stepsize

        # retrieve total_times incl. missing values
        times = self.runtimes
        # at the beginning all problems are unsolved by the empty schedule
        unsolved_problems = set(range(len(self.problems)))
        # iterate while there's still time left or until all problems are solved
        while overall_runtime < self.plantime and unsolved_problems:
            # try to retrieve the scores for each config in the current
            # timeslot interval
            runtimes = [current_timeslot for _ in range(len(self.algorithms))]
            configs_scores = self.evaluator.configs_scores(
                runtimes, list(unsolved_problems)
            )
            if not np.sum(configs_scores).astype(bool):
                # continue if there were no problems solved in this timeslot
                current_timeslot += self.stepsize
                continue
            best_config = np.argmax(configs_scores)

            # retreive all the problems ids for the best config;
            # problems_within_timeslot is always a 1d array
            problems_within_timeslot = np.where(
                (times[:, best_config] < current_timeslot + EPSILON)
                * (np.not_equal(times[:, best_config], None))
            )[0]
            # retreive all the problems solved in this timeslot
            solved_within_timeslot = unsolved_problems.intersection(
                problems_within_timeslot
            )
            # retreive the max runtime of the solved problems in this timeslot
            max_runtime = np.max(times[list(solved_within_timeslot), best_config])
            max_runtime = int(max_runtime + 1)

            # include the runtime into the schedule
            if best_config in used_configs:
                # best config has already been added to the schedule
                overall_runtime -= schedule_runtimes[used_configs[best_config]]
                max_runtime = (
                    max_runtime
                    if (overall_runtime + max_runtime) <= self.plantime
                    else (self.plantime - overall_runtime)
                )
                schedule_configs[used_configs[best_config]] = best_config
                schedule_runtimes[used_configs[best_config]] = max_runtime
            else:
                # best config id is new in the schedule
                max_runtime = (
                    max_runtime
                    if (overall_runtime + max_runtime) <= self.plantime
                    else (self.plantime - overall_runtime)
                )
                schedule_configs.append(best_config)
                schedule_runtimes.append(max_runtime)
                used_configs[best_config] = len(schedule_configs) - 1
            overall_runtime += max_runtime

            # increase the timeslot
            current_timeslot += self.stepsize
            # get rid of the solved problems
            unsolved_problems -= solved_within_timeslot
            if len(unsolved_problems) == 0:
                unsolved_problems = None

        self.schedule_config_ids = np.array(schedule_configs)
        self.schedule_runtimes = np.array(schedule_runtimes, int)
