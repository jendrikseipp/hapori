#! /usr/bin/env python

import argparse
import os
import subprocess
import sys


DIR = os.path.dirname(os.path.realpath(__file__))


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("domainfile")
    parser.add_argument("problemfile")
    parser.add_argument("planfile")
    parser.add_argument("config", help="one of {fdms1, fdms2}")
    parser.add_argument("timelimit")
    return parser.parse_args()


def main():
    args = parse_args()
    if args.config == "fdms1":
        merge_strategy = "merge_strategy=merge_sccs(order_of_sccs=topological,merge_selector=score_based_filtering(scoring_functions=[goal_relevance,dfp,total_order(atomic_ts_order=reverse_level,product_ts_order=new_to_old,atomic_before_product=false)]))"
    elif args.config == "fdms2":
        merge_strategy = "merge_strategy=merge_stateless(merge_selector=score_based_filtering(scoring_functions=[sf_miasm(shrink_strategy=shrink_bisimulation(greedy=false),max_states=50000,threshold_before_merge=1),total_order(atomic_ts_order=reverse_level,product_ts_order=new_to_old,atomic_before_product=false)]))"
    else:
        sys.exit("unknown config {}".format(args.config))
    subprocess.call([sys.executable, os.path.join(DIR, "fast-downward.py"),
        "--transform-task", "/planner/builds/release64/bin/preprocess",
        "--build", 'release64',
        "--search-memory-limit", '7744M',
        "--plan-file", args.planfile,
        args.domainfile, args.problemfile,
        "--symmetries", 'sym=structural_symmetries(search_symmetries=dks)',
        "--search",
        "astar(merge_and_shrink(shrink_strategy=shrink_bisimulation(greedy=false),{},label_reduction=exact(before_shrinking=true,before_merging=false),max_states=50000,threshold_before_merge=1,max_time={},verbosity=normal),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)".format(merge_strategy, args.timelimit)])


if __name__ == "__main__":
    main()
