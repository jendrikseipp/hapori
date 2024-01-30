#! /usr/bin/env python3

"""
Example experiment for running Singularity/Apptainer planner images.

The time and memory limits set with Lab can be circumvented by solvers that fork
child processes. Their resource usage is not checked. If you're running solvers
that don't check their resource usage like Fast Downward, we recommend using
cgroups or the "runsolver" tool to enforce resource limits. Since setting time
limits for solvers with cgroups is difficult, the experiment below uses the
``runsolver`` tool, which has been used in multiple SAT competitions to enforce
resource limits. For the experiment to run, the runsolver binary needs to be on
the PATH. You can obtain a runsolver copy from
https://github.com/jendrikseipp/runsolver.

Since Singularity (and Apptainer) reserve 1-2 GiB of *virtual* memory when
starting the container, we recommend either enforcing a higher virtual memory
limit with ``runsolver`` or limiting RSS memory with ``runsolver`` (like below).
For limiting RSS memory, you can also use `runlim
<https://github.com/arminbiere/runlim>`_, which is more actively maintained than
runsolver.

A note on running Singularity on clusters: reading large Singularity files over
the network is not optimal, so we recommend copying the images to a local
filesystem (e.g., /tmp/) before running experiments.
"""

import math
import os
from pathlib import Path
import platform
import sys

from portfolio_parser_runlim import get_parser

from downward import suites
from downward.reports.absolute import AbsoluteReport
from lab.environments import BaselSlurmEnvironment, LocalEnvironment
from lab.experiment import Experiment

import project

sys.path.append(str(Path(__file__).parent.parent))
import plan

# Create custom report class with suitable info and error attributes.
class BaseReport(AbsoluteReport):
    INFO_ATTRIBUTES = []
    ERROR_ATTRIBUTES = [
        "domain",
        "problem",
        "algorithm",
        "unexplained_errors",
        "error",
        "node",
    ]


NODE = platform.node()
RUNNING_ON_CLUSTER = NODE.endswith((".scicore.unibas.ch", ".cluster.bc2.ch"))
DIR = Path(__file__).resolve().parent
REPO = project.get_repo_base()
IMAGE = REPO / "images" / "2024-01-30-hapori_ibacop.sif"
assert IMAGE.is_file(), IMAGE
BENCHMARKS_DIR = REPO / "benchmarks"
assert BENCHMARKS_DIR.is_dir(), BENCHMARKS_DIR

MEMORY_LIMIT = 8000  # MiB
if RUNNING_ON_CLUSTER:
    SUITE = []
    ENVIRONMENT = BaselSlurmEnvironment(
        partition="infai_2",
        email="silvan.sievers@unibas.ch",
        # The limit of 3947 MiB is a virtual memory size limit set
        # externally (by slurm?). This can be observed using
        # resource.getrlimit(resource.RLIMIT_AS). So it seems
        # reasonable to use this as a default limit.
        memory_per_cpu="6354M",
        cpus_per_task=2,
        # paths obtained via:
        # $ module purge
        # $ module -q load Python/3.10.4-GCCcore-11.3.0
        # $ module -q load GCC/11.3.0
        # $ module -q load CMake/3.23.1-GCCcore-11.3.0
        # $ echo $PATH
        # $ echo $LD_LIBRARY_PATH
        setup='export PATH=/scicore/soft/apps/CMake/3.23.1-GCCcore-11.3.0/bin:/scicore/soft/apps/libarchive/3.6.1-GCCcore-11.3.0/bin:/scicore/soft/apps/cURL/7.83.0-GCCcore-11.3.0/bin:/scicore/soft/apps/Python/3.10.4-GCCcore-11.3.0/bin:/scicore/soft/apps/OpenSSL/1.1/bin:/scicore/soft/apps/XZ/5.2.5-GCCcore-11.3.0/bin:/scicore/soft/apps/SQLite/3.38.3-GCCcore-11.3.0/bin:/scicore/soft/apps/Tcl/8.6.12-GCCcore-11.3.0/bin:/scicore/soft/apps/ncurses/6.3-GCCcore-11.3.0/bin:/scicore/soft/apps/bzip2/1.0.8-GCCcore-11.3.0/bin:/scicore/soft/apps/binutils/2.38-GCCcore-11.3.0/bin:/scicore/soft/apps/GCCcore/11.3.0/bin:/infai/sieverss/repos/bin:/infai/sieverss/local:/export/soft/lua_lmod/centos7/lmod/lmod/libexec:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:$PATH\nexport LD_LIBRARY_PATH=/scicore/soft/apps/libarchive/3.6.1-GCCcore-11.3.0/lib:/scicore/soft/apps/cURL/7.83.0-GCCcore-11.3.0/lib:/scicore/soft/apps/Python/3.10.4-GCCcore-11.3.0/lib:/scicore/soft/apps/OpenSSL/1.1/lib:/scicore/soft/apps/libffi/3.4.2-GCCcore-11.3.0/lib64:/scicore/soft/apps/GMP/6.2.1-GCCcore-11.3.0/lib:/scicore/soft/apps/XZ/5.2.5-GCCcore-11.3.0/lib:/scicore/soft/apps/SQLite/3.38.3-GCCcore-11.3.0/lib:/scicore/soft/apps/Tcl/8.6.12-GCCcore-11.3.0/lib:/scicore/soft/apps/libreadline/8.1.2-GCCcore-11.3.0/lib:/scicore/soft/apps/ncurses/6.3-GCCcore-11.3.0/lib:/scicore/soft/apps/bzip2/1.0.8-GCCcore-11.3.0/lib:/scicore/soft/apps/binutils/2.38-GCCcore-11.3.0/lib:/scicore/soft/apps/zlib/1.2.12-GCCcore-11.3.0/lib:/scicore/soft/apps/GCCcore/11.3.0/lib64')
    OPT_TIME_LIMIT = 1800
    SAT_TIME_LIMIT = 1800
    AGL_TIME_LIMIT = 300
else:
    SUITE = ["miconic-strips:0-p01.pddl"]
    ENVIRONMENT = LocalEnvironment(processes=4)
    OPT_TIME_LIMIT = 15
    SAT_TIME_LIMIT = 15
    AGL_TIME_LIMIT = 15

ATTRIBUTES = [
    "cost",
    # "plan_length",
    "coverage",
    "error",
    "run_dir",
    "cpu_time",
    "wall_time",
    "used_memory",
    "solver_status_str",
    "solver_status_num",
    "invalid_plan",
    "memory_limit",
    "time_limit",
]


def get_time_limit(portfolio):
    track = portfolio[-3:]
    assert track in ["opt", "sat", "agl"], track
    match track:
        case "opt":
            return OPT_TIME_LIMIT
        case "sat":
            return SAT_TIME_LIMIT
        case "agl":
            return AGL_TIME_LIMIT
        case _:
            sys.exit(f"unknown track {track}")


def main():
    exp = Experiment(environment=ENVIRONMENT)
    exp.add_step("build", exp.build)
    exp.add_step("start", exp.start_runs)
    exp.add_step("parse", exp.parse)
    exp.add_fetcher(name="fetch", merge=False)

    exp.add_parser(get_parser())
    exp.add_resource("image", IMAGE, symlink=True)
    exp.add_resource("run_plan", REPO / "plan.py")
    exp.add_resource("fd_2018_configs", REPO / "configs/fd_2018_configs.py")
    exp.add_resource("filter_stderr", DIR / "filter-stderr.py")
    exp.add_resource("run_validate", "run-validate.sh")

    portfolios = [
        'hapori-greedy-agl',
        'hapori-greedy-opt',
        'hapori-greedy-sat',
        'hapori-miplan-opt',
        'hapori-miplan-random-agl',
        'hapori-miplan-sat',
        'hapori-stonesoup-agl',
        'hapori-stonesoup-opt',
        'hapori-stonesoup-sat',
        'hapori-uniform-agl',
        'hapori-uniform-opt',
        'hapori-uniform-sat',
    ]

    global SUITE
    for portfolio in portfolios:
        time_limit = get_time_limit(portfolio)
        algorithm_name = f"{portfolio}"
        if not SUITE:
            if 'opt' in portfolio:
                SUITE = project.SUITE_IPC23_OPT
            else:
                SUITE = project.SUITE_IPC23_SAT_AGL
        for task in suites.build_suite(BENCHMARKS_DIR, SUITE):
            run = exp.add_run()
            run.add_resource("domain", task.domain_file, "domain.pddl")
            run.add_resource("problem", task.problem_file, "problem.pddl")
            # Use runlim to limit time and memory. It must be on the system
            # PATH.
            internal_memory_limit = math.ceil(MEMORY_LIMIT*0.9) # generously keep some memory for running the portfolio driver
            internal_time_limit = math.ceil(time_limit*0.99) # use a slightly smaller runtime limit for the portfolio
            run.add_command(
                "run-planner",
                [
                    "runlim",
                    "--output-file=runlim.txt",
                    f"--time-limit={time_limit}",
                    f"--space-limit={MEMORY_LIMIT}",
                    "--propagate",
                    "{image}",
                    "{domain}",
                    "{problem}",
                    "sas_plan",
                    internal_memory_limit,
                    internal_time_limit,
                    portfolio,
                ],
            )
            run.add_command("run-validate", ["{run_validate}", "{domain}", "{problem}", "sas_plan"])
            # Remove temporary files from old Fast Downward versions.
            run.add_command("rm-tmp-files", ["rm", "-f", "output.sas", "output"])
            run.add_command("filter-stderr", [sys.executable, "{filter_stderr}"])

            run.set_property("domain", task.domain)
            run.set_property("problem", task.problem)
            run.set_property("algorithm", algorithm_name)
            run.set_property("id", [algorithm_name, task.domain, task.problem])

    report = Path(exp.eval_dir) / f"{exp.name}.html"
    exp.add_report(BaseReport(attributes=ATTRIBUTES), outfile=report)
    exp.run_steps()

if __name__ == "__main__":
    main()
