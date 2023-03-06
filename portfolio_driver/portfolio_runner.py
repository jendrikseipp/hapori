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
import subprocess
import sys

from . import call
from . import limits
from . import returncodes
from . import util


DIR = Path(__file__).resolve().parent
REPO = DIR.parent
DEFAULT_TIMEOUT = 1800


def run_search(image, planner, domain_file, problem_file, plan_manager, time, memory):
    dispatch = REPO / "plan.py"
    complete_args = [sys.executable, str(dispatch), image, domain_file, problem_file, plan_manager.get_plan_prefix()]
    if planner:
        complete_args += ["--config", planner]
    print("args: %s" % complete_args)

    try:
        exitcode = call.check_call(
            "search", complete_args, time_limit=time, memory_limit=memory)
    except subprocess.CalledProcessError as err:
        exitcode = err.returncode
    print("exitcode: %d" % exitcode)
    print()
    return exitcode


def compute_run_time(timeout, configs, pos):
    remaining_time = timeout - util.get_elapsed_time()
    print("remaining time: {}".format(remaining_time))
    relative_time = configs[pos][0]
    remaining_relative_time = sum(config[0] for config in configs[pos:])
    print("config {}: relative time {}, remaining {}".format(
          pos, relative_time, remaining_relative_time))
    return limits.round_time_limit(remaining_time * relative_time / remaining_relative_time)


def run_sat_config(configs, pos, domain_file, problem_file, plan_manager, timeout, memory):
    run_time = compute_run_time(timeout, configs, pos)
    if run_time <= 0:
        return None
    _, image, planner = configs[pos]
    result = run_search(image, planner, domain_file, problem_file, plan_manager, run_time, memory)
    plan_manager.process_new_plans()
    return result


def run_sat(configs, domain_file, problem_file, plan_manager, timeout, memory):
    for pos, (relative_time, image, planner) in enumerate(configs):
        exitcode = run_sat_config(
            configs, pos, domain_file, problem_file, plan_manager, timeout, memory)
        if exitcode is None:
            return

        yield exitcode

        if exitcode == returncodes.SUCCESS and plan_manager.abort_portfolio_after_first_plan():
            return


def run_opt(configs, domain_file, problem_file, plan_manager, timeout, memory):
    for pos, (relative_time, image, planner) in enumerate(configs):
        run_time = compute_run_time(timeout, configs, pos)
        if run_time <= 0:
            return
        exitcode = run_search(image, planner, domain_file, problem_file, plan_manager,
                              run_time, memory)
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


def get_portfolio_attributes(portfolio):
    attributes = {}
    with open(portfolio, "rb") as portfolio_file:
        content = portfolio_file.read()
        try:
            exec(content, attributes)
        except Exception as e:
            print(e)
            returncodes.exit_with_driver_critical_error(
                f"The portfolio {portfolio} could not be loaded: {e}.")
    if "PLANNERS" not in attributes:
        returncodes.exit_with_driver_critical_error("portfolios must define PLANNERS")
    return attributes


def run(portfolio, domain_file, problem_file, plan_manager, time, memory):
    """
    Run the configs in the given portfolio file.

    The portfolio is allowed to run for at most *time* seconds and may
    use a maximum of *memory* bytes.
    """
    attributes = get_portfolio_attributes(portfolio)
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

    if track == "opt":
        exitcodes = run_opt(
            configs, domain_file, problem_file, plan_manager, timeout, memory)
    else:
        exitcodes = run_sat(
            configs, domain_file, problem_file, plan_manager, timeout, memory)
    return returncodes.generate_portfolio_exitcode(list(exitcodes))
