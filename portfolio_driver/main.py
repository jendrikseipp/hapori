import logging
import os
import sys

from . import aliases
from . import arguments
from . import limits
from . import portfolio_runner
from . import util
from .plan_manager import PlanManager


def run_portfolio(args):
    logging.info("Running alias (%s)." % args.alias)
    time_limit = limits.get_time_limit(
        args.overall_time_limit)
    memory_limit = limits.get_memory_limit(
        args.overall_memory_limit)

    plan_manager = PlanManager(
        args.plan_file,
        portfolio_bound=args.portfolio_bound,
        single_plan=args.portfolio_single_plan)
    plan_manager.delete_existing_plans()

    assert args.portfolio
    logging.info("search portfolio: %s" % args.portfolio)
    domain_file = args.domain_file
    problem_file = args.problem_file
    return portfolio_runner.run(
        args.portfolio, domain_file, problem_file, plan_manager,
        time_limit, memory_limit)


def main():
    args = arguments.parse_args()
    logging.basicConfig(level=getattr(logging, args.log_level.upper()),
                        format="%(levelname)-8s %(message)s",
                        stream=sys.stdout)
    logging.debug("processed args: %s" % args)

    limits.print_limits("planner", args.overall_time_limit, args.overall_memory_limit)
    print()

    exitcode = None
    (exitcode, continue_execution) = run_portfolio(args)

    try:
        logging.info(f"Planner time: {util.get_elapsed_time():.2f}s")
    except NotImplementedError:
        # Measuring the runtime of child processes is not supported on Windows.
        pass

    # Exit with the exit code of the last component that ran successfully.
    # This means for example that if no plan was found, validate is not run,
    # and therefore the return code is that of the search.
    sys.exit(exitcode)


if __name__ == "__main__":
    main()
