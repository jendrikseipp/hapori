import argparse
import os.path
import re
import sys

from . import aliases
from . import returncodes
from . import util


DESCRIPTION = """Truncated Fast Downward driver script for running portfolios."""

LIMITS_HELP = """You can limit the time or memory.

Limits are given in seconds or MiB. You can change the unit by using the
suffixes s, m, h and K, M, G.

By default, all limits are inactive. Only external limits (e.g. set with
ulimit) are respected.

Portfolios require that a time limit is in effect. Portfolio configurations
that exceed their time or memory limit are aborted, and the next
configuration is run."""

COMPONENTS_PLUS_OVERALL = ["overall"]


"""
Function to emulate the behavior of ArgumentParser.error, but with our
custom exit codes instead of 2.
"""
def print_usage_and_exit_with_driver_input_error(parser, msg):
    parser.print_usage()
    returncodes.exit_with_driver_input_error("{}: error: {}".format(os.path.basename(sys.argv[0]), msg))


class RawHelpFormatter(argparse.HelpFormatter):
    """Preserve newlines and spacing."""
    def _fill_text(self, text, width, indent):
        return ''.join([indent + line for line in text.splitlines(True)])

    def _format_args(self, action, default_metavar):
        """Show explicit help for remaining args instead of "..."."""
        if action.nargs == argparse.REMAINDER:
            return "INPUT_FILE1 [INPUT_FILE2] [COMPONENT_OPTION ...]"
        else:
            return argparse.HelpFormatter._format_args(self, action, default_metavar)


def _check_mutex_args(parser, args):
    for pos, (name1, is_specified1) in enumerate(args):
        for name2, is_specified2 in args[pos + 1:]:
            if is_specified1 and is_specified2:
                print_usage_and_exit_with_driver_input_error(
                    parser, "cannot combine %s with %s" % (name1, name2))


def _get_time_limit_in_seconds(limit, parser):
    match = re.match(r"^(\d+)(s|m|h)?$", limit, flags=re.I)
    if not match:
        print_usage_and_exit_with_driver_input_error(parser, "malformed time limit parameter: {}".format(limit))
    time = int(match.group(1))
    suffix = match.group(2)
    if suffix is not None:
        suffix = suffix.lower()
    if suffix == "m":
        time *= 60
    elif suffix == "h":
        time *= 3600
    return time


def _get_memory_limit_in_bytes(limit, parser):
    match = re.match(r"^(\d+)(k|m|g)?$", limit, flags=re.I)
    if not match:
        print_usage_and_exit_with_driver_input_error(parser, "malformed memory limit parameter: {}".format(limit))
    memory = int(match.group(1))
    suffix = match.group(2)
    if suffix is not None:
        suffix = suffix.lower()
    if suffix == "k":
        memory *= 1024
    elif suffix is None or suffix == "m":
        memory *= 1024 * 1024
    elif suffix == "g":
        memory *= 1024 * 1024 * 1024
    return memory


def set_time_limit_in_seconds(parser, args, component):
    param = component + "_time_limit"
    limit = getattr(args, param)
    if limit is not None:
        setattr(args, param, _get_time_limit_in_seconds(limit, parser))


def set_memory_limit_in_bytes(parser, args, component):
    param = component + "_memory_limit"
    limit = getattr(args, param)
    if limit is not None:
        setattr(args, param, _get_memory_limit_in_bytes(limit, parser))


def _convert_limits_to_ints(parser, args):
    for component in COMPONENTS_PLUS_OVERALL:
        set_time_limit_in_seconds(parser, args, component)
        set_memory_limit_in_bytes(parser, args, component)


def parse_args():
    parser = argparse.ArgumentParser(
        description=DESCRIPTION,
        formatter_class=RawHelpFormatter,
        add_help=False)

    help_options = parser.add_argument_group(
        title=("driver options that show information and exit "
               "(don't run planner)"))
    # We manually add the help option because we want to control
    # how it is grouped in the output.
    help_options.add_argument(
        "-h", "--help",
        action="help", default=argparse.SUPPRESS,
        help="show this help message and exit")

    limits = parser.add_argument_group(
        title="time and memory limits", description=LIMITS_HELP)
    for component in COMPONENTS_PLUS_OVERALL:
        limits.add_argument("--{}-time-limit".format(component))
        limits.add_argument("--{}-memory-limit".format(component))

    driver_other = parser.add_argument_group(
        title="other driver options")
    driver_other.add_argument(
        "--alias",
        help="run a config with an alias (e.g. seq-sat-lama-2011)")
    driver_other.add_argument(
        "--log-level", choices=["debug", "info", "warning"],
        default="info",
        help="set log level (most verbose: debug; least verbose: warning; default: %(default)s)")

    driver_other.add_argument(
        "--plan-file", metavar="FILE", required=True,
        help="write plan(s) to FILE (anytime configurations append .1, .2, ...)")

    driver_other.add_argument(
        "--portfolio", metavar="FILE",
        help="run a portfolio specified in FILE")
    driver_other.add_argument(
        "--portfolio-bound", metavar="VALUE", default=None, type=int,
        help="exclusive bound on plan costs (only supported for satisficing portfolios)")
    driver_other.add_argument(
        "--portfolio-single-plan", action="store_true",
        help="abort satisficing portfolio after finding the first plan")

    parser.add_argument("domain_file", help="domain file")
    parser.add_argument("problem_file", help="problem file")

    args = parser.parse_args()

    _check_mutex_args(parser, [
            ("--alias", args.alias is not None),
            ("--portfolio", args.portfolio is not None)])

    if args.alias is None and args.portfolio is None:
        print_usage_and_exit_with_driver_input_error(
            parser, "pass exactly one of --alias and --portfolio.")

    _convert_limits_to_ints(parser, args)

    if args.alias:
        try:
            aliases.set_options_for_alias(args.alias, args)
        except KeyError:
            print_usage_and_exit_with_driver_input_error(
                parser, "unknown alias: %r" % args.alias)

    if args.portfolio_bound is not None and not args.portfolio:
        print_usage_and_exit_with_driver_input_error(
            parser, "--portfolio-bound may only be used for portfolios.")
    if args.portfolio_bound is not None and args.portfolio_bound < 0:
        print_usage_and_exit_with_driver_input_error(
            parser, "--portfolio-bound must not be negative.")
    if args.portfolio_single_plan and not args.portfolio:
        print_usage_and_exit_with_driver_input_error(
            parser, "--portfolio-single-plan may only be used for portfolios.")

    return args
