#! /usr/bin/env python

import sys
from collections import defaultdict

import numpy as np

from portfolio import EPSILON, Portfolio


class DomainConfigValues:
    def __init__(self, domain, config, times, qualities):
        self.domain = domain
        self.config = config
        self.times = times
        self.qualities = qualities

        self.sorting = np.argsort(times, axis=0)
        self.sorted_times = times[self.sorting]

    def set_problems_solved(self, solved_probs):
        self.qualities[solved_probs] = 0

    def set_runtime(self, time):
        self.qualities[self.times <= time + EPSILON] = 0
        # np.nonzero returns a tuple of arrays, one for each dimension
        return np.nonzero(self.times <= time + EPSILON)[0]

    def get_tradeoff(self):
        # The values may have changed and have to be recalculated each time
        cum_qualities = np.cumsum(self.qualities[self.sorting])
        # np.nonzero returns a tuple of arrays, one for each dimension
        indices = np.nonzero(cum_qualities >= EPSILON)[0]
        if indices.size == 0:
            return 0, 0, []
        index = np.min(indices)
        time = self.sorted_times[index]
        # improvement may be zero
        improvement = np.sum(self.qualities[(self.times <= time + EPSILON)])
        solved_probs = np.nonzero(self.times <= time)[0]
        return improvement, time, solved_probs

    def get_possible_improvement(self):
        improvement, time, solved_probs = self.get_tradeoff()
        return improvement

    def __str__(self):
        parts = [self.domain, self.config, self.times, self.qualities]
        return " ".join(str(part) for part in parts)


class DomainwisePortfolio(Portfolio):
    def __init__(self, *args, **kwargs):
        Portfolio.__init__(self, *args, **kwargs)

        self.portfolio_name = "Domain-wise portfolio"
        self.report_descr = "A per-domain portfolio."

    def compute_portfolio(self):
        # replace missing times with infinity for the evaluator
        all_times = np.where(self.runtimes is None, np.inf, self.runtimes).astype(
            "float"
        )

        self.schedule_runtimes = [0 for config in self.algorithms]
        self.schedule_config_ids = list(range(len(self.algorithms)))

        print(self.algorithms)
        print(all_times)
        print(self.qualities)
        print(list(self.domains.keys()))

        self.values = defaultdict(dict)
        for _domain_number, domain in enumerate(self.domains.keys()):
            for config_number, config in enumerate(self.algorithms):
                rows = self.domain_to_problem_indices[domain]
                times = all_times[rows, config_number]
                qualities = self.qualities[rows, config_number]
                self.values[domain][config] = DomainConfigValues(
                    domain, config, times, qualities
                )

        remaining_domains = set(self.domains.keys())
        while remaining_domains:
            domain, gap = self.get_next_domain(sorted(remaining_domains))
            print("NEXT DOMAIN", domain, gap)
            improved = self.improve_domain(domain)
            if not improved:
                remaining_domains.remove(domain)

        print("Possible improvement:", self.get_possible_improvement())

        # Round to next integers at the very end.
        self.schedule_runtimes = [
            int(time + 1 - EPSILON) for time in self.schedule_runtimes
        ]

    def improve_domain(self, domain):
        print("SEARCHING CONFIG FOR", domain)

        config_id, max_improvement, min_time = self.get_fastest_config(domain)
        if config_id is None:
            print("No config could be found for", domain)
            return False

        if sum(self.schedule_runtimes) + min_time > self.plantime:
            print("Schedule already uses all available plan time:", end=" ")
            print(
                f"{sum(self.schedule_runtimes)} + {min_time} > {self.plantime}"
            )
            return False

        fastest_config = self.algorithms[config_id]
        print("FASTEST CONFIG", fastest_config, min_time)

        self.schedule_runtimes[config_id] += min_time
        self.update_solved_problems(fastest_config, config_id)
        return True

    def update_solved_problems(self, fastest_config, config_id):
        for domain in list(self.domains.keys()):
            solved_problems = self.values[domain][fastest_config].set_runtime(
                self.schedule_runtimes[config_id]
            )
            for config in self.algorithms:
                self.values[domain][config].set_problems_solved(solved_problems)

    def get_fastest_config(self, domain):
        min_time = sys.maxsize
        fastest_config_id = None
        max_improvement = None
        for config_id, config in enumerate(self.algorithms):
            values = self.values[domain][config]
            improvement, time, solved_probs = values.get_tradeoff()
            print(values, improvement, time, solved_probs)
            # Change if planner is faster or is equally fast and better
            if improvement > 0 and (
                time < min_time or (time == min_time and improvement > max_improvement)
            ):
                fastest_config_id = config_id
                min_time = time
                max_improvement = improvement

        return fastest_config_id, max_improvement, min_time

    def get_possible_improvement(self):
        improvement = 0
        for _domain, configs in list(self.values.items()):
            improvement += sum(
                values.get_possible_improvement() for values in list(configs.values())
            )
        return improvement

    def get_gap(self, domain):
        """Return the score that this domain could get if we could run all
        configs for 30 minutes sequentially minus the already obtained score.

        Normalizing the gap by the score_max seems to decrease the overall
        portfolio score.
        """
        score_max = self.get_max_domain_score(domain)
        score_now = self.get_domain_score(domain, self.schedule_runtimes)
        print("GAP", domain, score_max, score_now, score_max - score_now)
        return score_max - score_now

    def get_next_domain(self, remaining_domains):
        gaps = [(domain, self.get_gap(domain)) for domain in remaining_domains]
        return max(gaps, key=lambda domain_gap: domain_gap[1])
