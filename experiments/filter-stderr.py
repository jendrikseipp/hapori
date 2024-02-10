#! /usr/bin/env python

"""Filter lines from run.err that stem from "expected errors"."""

from pathlib import Path
import shutil


IGNORE_PATTERNS = [
    "CPU time limit exceeded",
    "std::bad_alloc",
    "WARNING: will ignore action costs",
    "differs from the one in the portfolio file",
    "Terminated",
    "Killed",
    "underlay of /etc/localtime required more than",
    "sched_setaffinity failed: : Invalid argument",
    "warning: error reading memory from procfile",
    "Using TensorFlow backend.", # delfi
    "ls: cannot access", # Somewhere symple1 calls an ls, symple2 doesn;t
    # Older planners that use the src/downward bash script print their
    # command line to stderr if terminated. We catch all lines of
    # the cmd string not covered by the generic "$@" < $TEMPFILE' below.
    # This applies to mercury2014 and merwin.
    # mercury2014
    "lazy_greedy(hrb,preferred=hrb,cost_type=1,reopen_closed=false)",
    "iterated([",
    "lazy_greedy([hrb,hlm],preferred=[hrb,hlm]),",
    "lazy_wastar([hrb,hlm],preferred=[hrb,hlm],w=5),",
    "lazy_wastar([hrb,hlm],preferred=[hrb,hlm],w=3),",
    "lazy_wastar([hrb,hlm],preferred=[hrb,hlm],w=2),",
    "lazy_wastar([hrb,hlm],preferred=[hrb,hlm],w=1)],",
    "repeat_last=true,continue_on_fail=true)",
    "iterated([",
    "lazy_greedy([hrb,hlm1],preferred=[hrb,hlm1],",
    "cost_type=1,reopen_closed=false),",
    "lazy_greedy([hrb,hlm2],preferred=[hrb,hlm2],",
    "reopen_closed=false),",
    "lazy_wastar([hrb,hlm2],preferred=[hrb,hlm2],w=5),",
    "lazy_wastar([hrb,hlm2],preferred=[hrb,hlm2],w=3),",
    "lazy_wastar([hrb,hlm2],preferred=[hrb,hlm2],w=2),",
    "lazy_wastar([hrb,hlm2],preferred=[hrb,hlm2],w=1)],",
    # merwin
    "lazy(open=alt([tiebreaking([hn, hrb]), single(hrb,pref_only=true), single(hlm1), single(hlm1,pref_only=true)], boost=1000), preferred=[hrb,hlm1],",
    "iterated([",
    "lazy(open=alt([tiebreaking([hn, hrb]), single(hrb,pref_only=true), single(hlm), single(hlm,pref_only=true)], boost=1000),preferred=[hrb,hlm]),",
    "lazy_wastar([hrb,hlm],preferred=[hrb,hlm],w=5),",
    "lazy_wastar([hrb,hlm],preferred=[hrb,hlm],w=3),",
    "lazy_wastar([hrb,hlm],preferred=[hrb,hlm],w=2),",
    "lazy_wastar([hrb,hlm],preferred=[hrb,hlm],w=1)],",
    "iterated([",
    "lazy(open=alt([tiebreaking([hn1, hrb]), single(hrb,pref_only=true), single(hlm1), single(hlm1,pref_only=true)], boost=1000), preferred=[hrb,hlm1],",
    "cost_type=1,reopen_closed=false),",
    "lazy(open=alt([tiebreaking([hn2, hrb]), single(hrb,pref_only=true), single(hlm2), single(hlm2,pref_only=true)], boost=1000), preferred=[hrb,hlm2],",
    "reopen_closed=false),",
    "lazy_wastar([hrb,hlm2],preferred=[hrb,hlm2],w=5),",
    "lazy_wastar([hrb,hlm2],preferred=[hrb,hlm2],w=3),",
    "lazy_wastar([hrb,hlm2],preferred=[hrb,hlm2],w=2),",
    "lazy_wastar([hrb,hlm2],preferred=[hrb,hlm2],w=1)],",
    '"$@" < $TEMPFILE',
]


def main():
    print("Running filter-stderr.py")
    stderr = Path("run.err")
    if stderr.is_file():
        need_to_filter = False
        filtered_content = []
        with open(stderr, "r") as f:
            for line in f:
                if any(pattern in line for pattern in IGNORE_PATTERNS):
                    need_to_filter = True
                else:
                    filtered_content.append(line)

        if need_to_filter:
            shutil.move(stderr, "run.err.bak")
            # We write an empty file if everything has been filtered. Lab
            # will remove empty run.err files later.
            with open(stderr, "w") as f:
                f.writelines(filtered_content)


if __name__ == "__main__":
    main()
