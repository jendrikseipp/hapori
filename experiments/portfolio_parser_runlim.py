import os
import re
import sys

from lab.parser import Parser


def coverage(content, props):
    props["coverage"] = int("cost" in props)
    props["claimed_coverage"] = int(any("Solution found" in line for line in content.splitlines()))
    if not props["coverage"] and props["claimed_coverage"]:
        props.add_unexplained_error(f"solver claims solution; we found no or an invalid plan")

def invalid_plan(content, props):
    props["invalid_plan"] = (content.find("Plan failed to execute") > -1 or
        content.find("Bad operator in plan!") > -1 or
        content.find("Bad plan description") > -1)

def set_outcome(content, props):
    solved = props["coverage"]

    if solved:
        props["error"] = "solved"
        return props

    if props["invalid_plan"]:
        props["error"] = "invalid_plan"
        return props

    # Delete records of runtime if the task was not solved.
    props["cpu_time"] = None
    props["wall_time"] = None

    # Returncode computed from portfolio driver or the abort from runlim.
    exit_code = props["planner_exit_code"]
    match exit_code:
        case 0:
            props.add_unexplained_error("exit code 0 but neither solved nor invalid plan")
        case [3, 122]:
            props["error"] = "out_of_memory"
        case [2, 123]:
            props["error"] = "out_of_time"
        case 124:
            props["error"] = "out_of_time_and_memory"
        case 125:
            props["error"] = "all_components_error"
        case 126:
            props["error"] = "error_and_out_of_memory"
        case 127:
            props["error"] = "error_and_out_of_time"
        case 128:
            props["error"] = "error_and_out_of_time_and_memory"
        case _:
            props.add_unexplained_error(f"unknown-portfolio-returncode {exit_code}")
            props["error"] = "unknown-portfolio-returncode"
    return props


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
    parser.add_function(set_outcome)
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
