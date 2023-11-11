#! /usr/bin/env python

"""
Script used for a quick analysis of the training data.
"""

import numpy as np

array = np.loadtxt("data/01-opt-planners-eval/runtimes.csv", delimiter=",", dtype=object)
header = array[0,1:]
pddl_names = array[1:,0]
domain_for_task = np.array([task.split(':')[0] for task in pddl_names])
domains = set(domain_for_task)

runtimes = array[1:,1:]
runtimes[runtimes == "-"] = 10000
runtimes = runtimes.astype(float)

# print(",".join(header))
aggregated_domain_coverage = []
for domain in sorted(domains):
    mask = domain_for_task == domain
    domain_runtimes = runtimes[mask]

    coverage = domain_runtimes < 1000
    coverage = coverage.sum(axis=0)
    aggregated_domain_coverage.append(coverage)

    # print(",".join([str(x) for x in coverage]))
    # for planner, coverage in sorted(zip(header, coverage), key=lambda x: -x[1]):
        # print(planner, coverage)
aggregated_domain_coverage = np.stack(aggregated_domain_coverage)
print([f"{m:.1f}+-{s:.1f}" for m, s in zip(aggregated_domain_coverage.mean(axis=0), aggregated_domain_coverage.std(axis=0))])


# order = coverage.argsort()
# sorted_header = header[order]
# sorted_coverage = coverage[order]
# for planner, coverage in zip(sorted_header, sorted_coverage):
    # print(planner, coverage)
