import os
import re
import sys

from lab.parser import Parser


def coverage(content, props):
    props["coverage"] = int("cost" in props)
    props["claimed_coverage"] = int(any("Solution found" in line for line in content.splitlines()))
    if not props["coverage"] and props["claimed_coverage"]:
        props.add_unexplained_error(f"solver claims solution; we found no plan")

def invalid_plan(content, props):
    props["invalid_plan"] = (content.find("Plan failed to execute") > -1 or
        content.find("Bad operator in plan!") > -1 or
        content.find("Bad plan description") > -1)

def set_outcome(content, props):
    lines = content.splitlines()
    solved = props["coverage"]
    out_of_time = 0
    out_of_memory = 0
    out_of_time_or_memory = 0
    exit_code = props["planner_exit_code"]
    runlim_status = props.get("solver_status_num", 0) # runlim file can be incomplete if the run crashes
    if exit_code == 2 or exit_code == 123 or runlim_status == 2:
        out_of_time = 1
    if exit_code == 3 or exit_code == 122 or runlim_status == 3:
        out_of_memory = 1
    invalid_plan = props["invalid_plan"]
    # print(solved, out_of_time, out_of_memory, invalid_plan)
    if (
        not solved
        and not out_of_time
        and not out_of_memory
        and not invalid_plan):
        # use 0 for time/memory if runlim file is incomplete
        cpu_time = props.get("cpu_time", 0)
        used_memory = props.get("used_memory", 0)
        if cpu_time > props["time_limit"]:
            out_of_time = 1
        elif cpu_time > 1750 and used_memory > 7000:
            out_of_time_or_memory = 1
        elif cpu_time > 1750:
            out_of_time = 1
        elif used_memory > 7000:
            out_of_memory = 1

    # In cases where CPU time is very slightly above the threshold so that
    # runlim didn't kill the planner yet and the planner solved a task
    # just within the limit, runsolver will still record an "out of time".
    # We remove this record. This case also applies to iterative planners.
    # If such planners solve the task, we don't treat them as running out
    # of time.
    if solved and (out_of_time or out_of_memory):
        print("task solved however runlim recorded an out_of_*")
        # print(props)
        out_of_time = 0
        out_of_memory = 0

    if not solved:
        props["cpu_time"] = None
        props["wall_time"] = None

    # print(solved, out_of_time, out_of_memory, out_of_time_or_memory, invalid_plan)
    if solved ^ out_of_time ^ out_of_memory ^ out_of_time_or_memory ^ invalid_plan:
        if solved:
            props["error"] = "solved"
        elif out_of_time:
            props["error"] = "out_of_time"
        elif out_of_memory:
            props["error"] = "out_of_memory"
        elif out_of_time_or_memory:
            props["error"] = "out_of_time_or_memory"
        elif invalid_plan:
            props["error"] = "invalid_plan"
    else:
        props.add_unexplained_error(f"could not determine outcome")
        props["error"] = "unknown-outcome"


def type_int_or_none(elem):
    return None if elem is None else int(elem)


def get_parser():
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
        "time_limit", r"\[runlim\] time limit:\t*(\d+) seconds",
        type=int, file="runlim.txt", required=True,
    )
    parser.add_pattern(
        "memory_limit", r"\[runlim\] space limit:\t*(\d+) MB",
        type=int, file="runlim.txt", required=True,
    )
    # Cumulative runtime and memory of the solver and all child processes.
    parser.add_pattern(
        "wall_time", r"\[runlim\] real:\t*(.+) seconds",
        type=float, file="runlim.txt", required=True
    )
    parser.add_pattern(
        "cpu_time", r"\[runlim\] time:\t*(.+) seconds",
        type=float, file="runlim.txt", required=True
    )
    parser.add_pattern(
        "used_memory", r"\[runlim\] space:\t*(\d+) MB",
        type=int, file="runlim.txt",
        required=True
    )
    parser.add_pattern(
        "solver_status_str", r"\[runlim\] status:\t*(.+)",
        type=str, file="runlim.txt",
        required=True
    )
    parser.add_pattern(
        "solver_status_num", r"\[runlim\] result:\t*(\d+)",
        type=int, file="runlim.txt",
        required=True
    )

    # parser.add_pattern("plan_length", r"\nFinal value: (\S+)(?: \S+)?\n", type=int)
    # parser.add_pattern("plan_length", r"\nFinal value: (.+?) (?:.+)?\n", type=int)
    parser.add_pattern("cost", r"Final value: (\d+)", type=type_int_or_none)
    parser.add_function(coverage)
    parser.add_function(invalid_plan)
    # also parse errors removed by fitler-stderr.py
    parser.add_function(set_outcome, file="runlim.txt")
    return parser

# facility to test parsing files in the directory the script is called from
if __name__ == "__main__":
    from lab import tools
    from pathlib import Path
    parser = get_parser()
    props = tools.Properties("properties")
    run_dir = Path(".").resolve()
    parser.parse(run_dir, props)
    print(props)
    props.write()
