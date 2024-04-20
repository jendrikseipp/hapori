""" Module for running planner portfolios.

Memory limits: We apply the same memory limit that is given to the
plan script to each planner call. Note that this setup does not work if
the sum of the memory usage of the Python process and the planner calls
is limited. In this case the Python process might get killed although
we would like to kill only the single planner call and continue with
the remaining configurations. If we ever want to support this scenario
we will have to reduce the memory limit of the planner calls by the
amount of memory that the Python process needs. On maia for example
this amounts to 128MB of reserved virtual memory. We can make Python
reserve less space by lowering the soft limit for virtual memory before
the process is started.
"""

__all__ = ["run"]

from pathlib import Path
import itertools
import random
import re
import shutil
import string
import subprocess
import sys

from . import call
from . import limits
from . import returncodes
from . import util


DIR = Path(__file__).resolve().parent
REPO = DIR.parent


def run_search(planner, config, pos, domain_file, problem_file, plan_file, time, memory):
    dispatch = REPO / "plan.py"
    complete_args = []
    assert time is not None or memory is not None
    runlim_file = f"runlim-pos{pos}-{planner}-{config}.txt"
    complete_args.extend(["runlim", f"--output-file={runlim_file}", "--propagate"])
    if time is not None:
        complete_args.append(f"--time-limit={time}")
    if memory is not None:
        complete_args.append(f"--space-limit={int(limits.convert_to_mb(memory))}")
    complete_args += [sys.executable, str(dispatch), planner, domain_file, problem_file, plan_file, "--config", config]
    print("Hapori component args: %s" % complete_args)

    try:
        exitcode = call.check_call(
            "search", complete_args, time_limit=None, memory_limit=None)
    except subprocess.CalledProcessError as err:
        exitcode = err.returncode
    print("Hapori component exitcode: %d" % exitcode)
    runlim_run_time = 0.0
    assert Path(runlim_file).exists(), f"{runlim_file} not found"
    with open(runlim_file, "r") as f:
        pattern = re.compile(r"\[runlim\] time:\t*(.+) seconds")
        match = pattern.search(f.read())
        if match:
            runlim_run_time = float(match.group(1))
            print(f"Hapori component runlim run time: {runlim_run_time}")
        else:
            sys.exit(f"Could not determine runlim run time from runlim file {runlim_run_time}")
    print()
    return exitcode, runlim_run_time


def compute_run_time(timeout, configs, pos, run_time_so_far):
    remaining_time = timeout - util.get_elapsed_time() - run_time_so_far
    print("remaining time: {:.2f}".format(remaining_time))
    relative_time = configs[pos][0]
    remaining_relative_time = sum(config[0] for config in configs[pos:])
    absolute_time_limit = limits.round_time_limit(remaining_time * relative_time / remaining_relative_time)
    print("config {}: relative time {}, remaining time {}, absolute time {}".format(
          pos, relative_time, remaining_relative_time, absolute_time_limit))
    return absolute_time_limit


def get_random_string(length):
    letters = string.ascii_lowercase
    result = ''.join(random.choice(letters) for i in range(length))
    return result


def run_multi_plan_portfolio(configs, domain_file, problem_file, plan_manager, timeout, memory):
    plan_counter = 1
    runlim_run_time_so_far = 0.0
    for pos, (relative_time, (planner, config)) in enumerate(configs):
        next_plan_prefix = f"config.{pos}.{planner}"
        run_time = compute_run_time(timeout, configs, pos, runlim_run_time_so_far)
        if run_time <= 0: # skip over portfolio components with too low time limit
            continue
        exitcode, used_run_time = run_search(
            planner, config, pos, domain_file, problem_file,
            next_plan_prefix, run_time, memory)
        runlim_run_time_so_far += used_run_time

        existing_plan_files = [str(plan) for plan in get_existing_plans(next_plan_prefix) ]
        # print(f"Component computed the following plan(s): {existing_plan_files}")
        for plan_file in existing_plan_files:
            print(f"Moving {plan_file} to {plan_manager.get_plan_prefix()}.{plan_counter}")
            shutil.move(plan_file, f"{plan_manager.get_plan_prefix()}.{plan_counter}")
            plan_counter += 1

        yield exitcode


def run_single_plan_portfolio(configs, domain_file, problem_file, plan_manager, timeout, memory):
    runlim_run_time_so_far = 0.0
    for pos, (relative_time, (planner, config)) in enumerate(configs):
        run_time = compute_run_time(timeout, configs, pos, runlim_run_time_so_far)
        if run_time <= 0: # skip over portfolio components with too low time limit
            continue
        exitcode, used_run_time = run_search(
            planner, config, pos, domain_file, problem_file,
            plan_manager.get_plan_prefix(), run_time, memory)
        runlim_run_time_so_far += used_run_time
        yield exitcode

        if exitcode == returncodes.SUCCESS:
            break


def get_track(portfolio):
    track_names = ["agl", "opt", "sat"]
    track = None
    for track_name in track_names:
        if Path(portfolio).stem.endswith(f"-{track_name}"):
            track = track_name
    if not track:
        returncodes.exit_with_driver_critical_error(f"portfolio paths must end with one of {track_names}")
    return track


def get_portfolio_attributes(portfolio, domain, problem, time):
    attributes = {
        'DOMAIN': domain,
        'PROBLEM': problem,
        'AVAIL_TIME': time,
    }
    with open(portfolio, "rb") as portfolio_file:
        content = portfolio_file.read()
        try:
            exec(content, attributes)
        except Exception as e:
            print(e)
            returncodes.exit_with_driver_critical_error(
                f"The portfolio {portfolio} could not be loaded: {e}.")
    if "TRACK" in attributes:
        returncodes.exit_with_driver_critical_error("portfolios must not define TRACK")
    if "PLANNERS" not in attributes:
        returncodes.exit_with_driver_critical_error("portfolios must define PLANNERS")
    return attributes


def get_existing_plans(plan_prefix):
    plan_prefix = Path(plan_prefix).resolve()
    if plan_prefix.exists():
        yield plan_prefix

    for counter in itertools.count(start=1):
        plan_filename = Path(f"{plan_prefix}.{counter}")
        if plan_filename.exists():
            yield plan_filename
        else:
            break


def run(portfolio, domain_file, problem_file, plan_manager, time, memory):
    """
    Run the configs in the given portfolio file.

    The portfolio is allowed to run for at most *time* seconds and may
    use a maximum of *memory* bytes.
    """
    attributes = get_portfolio_attributes(portfolio, domain_file, problem_file, time)
    configs = attributes["PLANNERS"]
    track = get_track(portfolio)

    if time is None:
        if sys.platform == "win32":
            returncodes.exit_with_driver_unsupported_error(limits.CANNOT_LIMIT_TIME_MSG)
        else:
            returncodes.exit_with_driver_input_error(
                "Portfolios need a time limit. Please pass --search-time-limit "
                "or --overall-time-limit to fast-downward.py.")

    timeout = util.get_elapsed_time() + time

    if track in {"agl", "opt"}:
        exitcodes = run_single_plan_portfolio(
            configs, domain_file, problem_file, plan_manager, timeout, memory)
    else:
        assert track == "sat", track
        exitcodes = run_multi_plan_portfolio(
            configs, domain_file, problem_file, plan_manager, timeout, memory)

    return returncodes.generate_portfolio_exitcode(list(exitcodes))
