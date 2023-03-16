#! /usr/bin/env python

import os
import re
import sys

from lab.parser import Parser


def fix_costs(content, props):
    if "plan_length" in props:
        if props["cost"] is None:
            props["cost"] = props["plan_length"]
        else:
            # HACK: At one point validate seems to have change the order in its
            # output.
            # Assume all actions have cost at least 1!
            cost, length = props["cost"], props["plan_length"]
            props["plan_length"] = min(cost, length)
            props["cost"] = max(cost, length)
    elif "cost" in props:
        del props["cost"]


def coverage(content, props):
    props["coverage"] = int("cost" in props)

def invalid_plan(content, props):
    props["invalid_plan"] = content.find("Plan failed to execute") > -1 or content.find("Bad operator in plan!") > -1

def unsupported(content, props):
    if "unsupported" in props and props["unsupported"]:
        return
    if (content.find(" does not support axioms!") > -1 or
            content.find("axioms not supported") > -1 or
            content.find("Tried to use unsupported feature") > -1 or
            content.find("This configuration does not support") > -1 or
            content.find("No factoring found!") > -1):
        props["unsupported"] = True
    else:
        props["unsupported"] = False

def set_outcome(content, props):
    lines = content.splitlines()
    solved = props["coverage"]
    unsolvable = False  # assume all tasks are solvable
    unsupported = props["unsupported"]
    out_of_time = int("TIMEOUT=true" in lines)
    out_of_memory = int("MEMOUT=true" in lines)
    invalid_plan = props["invalid_plan"]
    # runsolver decides "out of time" based on CPU rather than (cumulated)
    # WCTIME.
    if (
        not solved
        and not unsolvable
        and not out_of_time
        and not out_of_memory
        and not unsupported
        and not invalid_plan
        and props["cpu_time"] > props["time_limit"]
    ):
        out_of_time = 1
    # In cases where CPU time is very slightly above the threshold so that
    # runsolver didn't kill the planner yet and the planner solved a task
    # just within the limit, runsolver will still record an "out of time".
    # We remove this record. This case also applies to iterative planners.
    # If such planners solve the task, we don't treat them as running out
    # of time.
    if (solved or unsolvable) and (out_of_time or out_of_memory):
        print("task solved however runsolver recorded an out_of_*")
        print(props)
        out_of_time = 0
        out_of_memory = 0

    if not solved and not unsolvable:
        props["cpu_time"] = None
        props["wall_time"] = None

    print(solved, unsolvable, out_of_time, out_of_memory, unsupported, invalid_plan)
    if solved ^ unsolvable ^ out_of_time ^ out_of_memory ^ unsupported ^ invalid_plan:
        if solved:
            props["error"] = "solved"
        elif unsolvable:
            props["error"] = "unsolvable"
        elif out_of_time:
            props["error"] = "out_of_time"
        elif out_of_memory:
            props["error"] = "out_of_memory"
        elif unsupported:
            props["error"] = "unsupported"
        elif invalid_plan:
            props["error"] = "invalid_plan"
    else:
        print(f"unexpected error: {props}", file=sys.stderr)
        props["error"] = "unexpected-error"


def type_int_or_none(elem):
    return None if elem is None else int(elem)


def main():
    print("Running singularity parser")
    parser = Parser()
    parser.add_pattern(
        "planner_exit_code",
        r"run-planner exit code: (.+)\n",
        type=int,
        file="driver.log",
        required=True,
    )
    parser.add_pattern(
        "node", r"node: (.+)\n", type=str, file="driver.log", required=True
    )
    parser.add_pattern(
        "planner_wall_clock_time",
        r"run-planner wall-clock time: (.+)s",
        type=float,
        file="driver.log",
        required=True,
    )
    parser.add_pattern(
        "time_limit",
        r"Enforcing CPUTime limit \(soft limit, will send "
        r"SIGTERM then SIGKILL\): (\d+) seconds",
        type=int,
        file="watch.log",
        required=True,
    )
    # Cumulative runtime and virtual memory of the solver and all child processes.
    parser.add_pattern(
        "wall_time", r"WCTIME=(.+)", type=float, file="values.log", required=True
    )
    parser.add_pattern(
        "cpu_time", r"CPUTIME=(.+)", type=float, file="values.log", required=True
    )
    parser.add_pattern(
        "virtual_memory", r"MAXVM=(\d+)", type=int, file="values.log", required=True
    )
    parser.add_pattern(
        "used_memory", r"MAXMM=(\d+)", type=int, file="values.log",
        required=True
    )
    parser.add_pattern(
        "resident_memory", r"MAXRSS=(\d+)", type=int, file="values.log",
        required=True
    )

    # parser.add_pattern("plan_length", r"\nFinal value: (\S+)(?: \S+)?\n", type=int)
    parser.add_pattern("plan_length", r"\nFinal value: (.+?) (?:.+)?\n", type=int)
    parser.add_pattern("cost", r"\nFinal value: .+? (.+)?\n", type=type_int_or_none)
    parser.add_function(fix_costs)
    parser.add_function(coverage)
    parser.add_function(invalid_plan)
    if os.path.exists("run.log"):
        parser.add_function(unsupported, file="run.log")
    if os.path.exists("run.err"):
        parser.add_function(unsupported, file="run.err")
    parser.add_function(set_outcome, file="values.log")
    parser.parse()


if __name__ == "__main__":
    main()
