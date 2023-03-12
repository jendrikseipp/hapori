#!/usr/bin/env python3
import json
import sys

IGNORE_PATTERNS = [
    "CPU time limit exceeded",
    "std::bad_alloc",
    "WARNING: will ignore action costs",
    "differs from the one in the portfolio file",
    "Terminated",
    "Killed",
    "underlay of /etc/localtime required more than",
    "sched_setaffinity failed: : Invalid argument",
    "This configuration does not support",
    "does not support axioms.",
    "axioms not supported",
    "BDDError",
    "Error: Parser failed to read file!",
    "Error in allocating",
    "Plan not valid!",
    "(soft limit:",
    "(hard limit)",
    "run.err: unexpected error: {",
    "std::length_error",
    "ls: cannot access"
]

KEY_UNEXPLAINED_ERRORS = "unexplained_errors"
def main(file_properties):
    with open(file_properties, "r") as f:
        properties = json.load(f)
    for props in properties.values():
        unexplained_errors = props.get(KEY_UNEXPLAINED_ERRORS)
        if unexplained_errors:
            new_unexplained_errors = [ue for ue in unexplained_errors
                                  if not any(ip in ue for ip in IGNORE_PATTERNS)]
            if len(new_unexplained_errors) == 0:
                del props[KEY_UNEXPLAINED_ERRORS]
            else:
                props[KEY_UNEXPLAINED_ERRORS] = new_unexplained_errors
    with open(file_properties, "w") as f:
        json.dump(properties, f, indent=4, sort_keys=True)
if __name__ == "__main__":
    for _f in sys.argv[1:]:
        main(_f)