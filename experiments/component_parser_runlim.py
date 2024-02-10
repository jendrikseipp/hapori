import os
from pathlib import Path
import re
import subprocess
import sys

from lab import tools
from lab.parser import Parser


def val_plan_too_long(content, props):
    props["val_plan_too_long"] = (
        content.find("Bad plan description") > -1 or
        content.find("Error occurred in validation attempt:\n  std::bad_alloc") > -1)

def val_invalid_plan(content, props):
    props["val_invalid_plan"] = (
        content.find("Plan failed to execute") > -1 or
        content.find("Bad operator in plan!") > -1 or
        content.find("Plan invalid") > -1
        )

def collect_plans_and_run_upv(content, props):
    props["upv_cost"] = None
    props["upv_invalid_plan"] = False
    properties_file = props.path
    run_dir = properties_file.parent
    plan_files = []
    for element in run_dir.iterdir():
        if element.is_file() and (element.name == "sas_plan" or element.name.startswith("sas_plan.")):
            plan_files.append(element.name)
    plan_files = sorted(plan_files)
    if plan_files:
        # print(f"running upv on {plan_files[-1]} in {run_dir}")
        completed_process = subprocess.run(["validate.bin", "domain.pddl", "problem.pddl", plan_files[-1]], cwd=run_dir, text=True, capture_output=True)
        for line in completed_process.stdout.splitlines():
            # upv uses colored output
            pattern = re.compile("\\x1b\[1;32mValue: (\d+)\.0*")
            match = pattern.match(line)
            if match:
                cost = int(match.group(1))
                props["upv_cost"] = cost
        for line in completed_process.stderr.splitlines():
            if "Error: Plan failed to execute" in line or "Error: Goal not satisifed - Plan invalid" in line :
                props["upv_invalid_plan"] = True
    props["plan_files"] = plan_files

def custom_errors_stdout(content, props):
    lines = content.splitlines()
    if any('Was not allowed to increase horizon length.' in line for line in lines): # mpc
        props["error"] = "out_of_memory"
    if any('alloc' in line for line in lines): # delfi
        props["error"] = "out_of_memory"
    return props

def custom_errors_stderr(content, props):
    lines = content.splitlines()
    if any("CPU time limit exceeded" in line for line in lines):
        props["error"] = "out_of_time"
    if any("alloc" in line for line in lines): # decstar, jasper, decstar, symba
        props["error"] = "out_of_memory"
    if any("std::length_error" in line for line in lines): # symba
        props["error"] = "out_of_memory"
    return props

def unsupported(content, props):
    if "unsupported" in props and props["unsupported"]:
        return
    if (content.find(" does not support axioms!") > -1 or
        content.find("axioms not supported") > -1 or
        content.find("Tried to use unsupported feature") > -1 or
        content.find("This configuration does not support") > -1 or
        content.find("No factoring found!") > -1 or
        content.find("WARNING: unsupported :requirement :derived-predicates") > -1 or # mpc
        content.find("Error: Parser failed to read file") > -1 or # VAL on some cavediving-adl tasks
        content.find("Planning task not solvable") > -1 or # probe
        content.find("currently does not support axioms") > -1 or # complementary2
        content.find("problem definitions on line") > -1 or # mpc
        content.find("syntax error") > -1 # mpc
        ):
        props["unsupported"] = True
    else:
        props["unsupported"] = False

def set_outcome(content, props):
    # determine coverage
    if "val_cost" in props:
        props["coverage"] = 1
        props["cost"] = props["val_cost"]
    elif props["upv_cost"] is not None:
        props["coverage"] = 1
        props["cost"] = props["upv_cost"]
    else:
        props["coverage"] = 0
        props["cost"] = None
    solved = props["coverage"]

    # check for invalid plans
    if props["val_invalid_plan"] or props["upv_invalid_plan"]:
        props["invalid_plan"] = True
    else:
        props["invalid_plan"] = False
    invalid_plan = props["invalid_plan"]

    # check for out of time and/or memory status
    out_of_time = int(props["solver_status_num"] == 2)
    out_of_memory = int(props["solver_status_num"] == 3)
    if not out_of_time and not out_of_memory:
        if props.get("error") == "out_of_time":
            out_of_time = 1
        if props.get("error") == "out_of_memory":
            out_of_memory = 1
    out_of_time_or_memory = 0

    # further possible outcome
    unsupported = props["unsupported"]

    # print(solved, out_of_time, out_of_memory, unsupported, invalid_plan)
    if (
        not solved
        and not out_of_time
        and not out_of_memory
        and not unsupported
        and not invalid_plan):
        if props["cpu_time"] > props["time_limit"]:
            out_of_time = 1
        elif props["cpu_time"]  > 1750 and props["used_memory"] > 7000:
            out_of_time_or_memory = 1
        elif props["cpu_time"]  > 1750:
            out_of_time = 1
        elif props["used_memory"] > 7000:
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

    # print(solved, out_of_time, out_of_memory, out_of_time_or_memory, unsupported, invalid_plan)
    if solved ^ out_of_time ^ out_of_memory ^ out_of_time_or_memory ^ unsupported ^ invalid_plan:
        if solved:
            props["error"] = "solved"
        elif out_of_time:
            props["error"] = "out_of_time"
        elif out_of_memory:
            props["error"] = "out_of_memory"
        elif out_of_time_or_memory:
            props["error"] = "out_of_time_or_memory"
        elif unsupported:
            props["error"] = "unsupported"
        elif invalid_plan:
            props["error"] = "invalid_plan"
    else:
        props["error"] = "crash"


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

    parser.add_pattern("val_cost", r"Final value: (\d+)", type=type_int_or_none)
    parser.add_function(val_plan_too_long)
    parser.add_function(val_invalid_plan)
    parser.add_function(collect_plans_and_run_upv)
    parser.add_function(custom_errors_stdout)
    parser.add_function(custom_errors_stderr, file="run.err")
    # also parse errors removed by fitler-stderr.py
    parser.add_function(custom_errors_stderr, file="run.err.bak")
    parser.add_function(unsupported)
    parser.add_function(unsupported, file="run.err")
    parser.add_function(set_outcome, file="runlim.txt")
    return parser

# facility to test parsing files in the directory the script is called from
if __name__ == "__main__":
    parser = get_parser()
    props = tools.Properties("properties")
    run_dir = Path(".").resolve()
    parser.parse(run_dir, props)
    print(props)
    props.write()
