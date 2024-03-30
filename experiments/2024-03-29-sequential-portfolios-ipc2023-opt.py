#! /usr/bin/env python3

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
IMAGE = REPO / "2024-02-09-hapori_sequential_portfolios.sif"
assert IMAGE.is_file(), IMAGE
IBACOP_IMAGE = REPO / "2024-03-29-hapori_ibacop2.sif"
assert IBACOP_IMAGE.is_file(), IBACOP_IMAGE
BENCHMARKS_DIR = REPO / "benchmarks"
assert BENCHMARKS_DIR.is_dir(), BENCHMARKS_DIR
PORTFOLIOS_DIR = REPO / "sequential-portfolios"
PORTFOLIOS = [
    "hapori-cluster-opt",
    "hapori-domain-wise-opt",
    "hapori-greedy-opt",
    "hapori-ibacop2-opt",
    "hapori-increasing-time-limit-opt",
    "hapori-miplan-hardest-opt",
    "hapori-randomized-iterative-search-opt",
    "hapori-stonesoup-opt",
    "hapori-uniform-opt",
]

MEMORY_LIMIT = 8000  # MiB
if RUNNING_ON_CLUSTER:
    SUITE = project.SUITE_IPC23_OPT_SMALL
    ENVIRONMENT = BaselSlurmEnvironment(
        partition="infai_3",
        email="silvan.sievers@unibas.ch",
        memory_per_cpu="3947M",
        cpus_per_task=3,
        # paths obtained via:
        # $ module purge
        # $ module -q load Python/3.10.4-GCCcore-11.3.0
        # $ module -q load GCC/11.3.0
        # $ module -q load CMake/3.23.1-GCCcore-11.3.0
        # $ echo $PATH
        # $ echo $LD_LIBRARY_PATH
        setup='export PATH=/scicore/soft/apps/CMake/3.23.1-GCCcore-11.3.0/bin:/scicore/soft/apps/libarchive/3.6.1-GCCcore-11.3.0/bin:/scicore/soft/apps/cURL/7.83.0-GCCcore-11.3.0/bin:/scicore/soft/apps/Python/3.10.4-GCCcore-11.3.0/bin:/scicore/soft/apps/OpenSSL/1.1/bin:/scicore/soft/apps/XZ/5.2.5-GCCcore-11.3.0/bin:/scicore/soft/apps/SQLite/3.38.3-GCCcore-11.3.0/bin:/scicore/soft/apps/Tcl/8.6.12-GCCcore-11.3.0/bin:/scicore/soft/apps/ncurses/6.3-GCCcore-11.3.0/bin:/scicore/soft/apps/bzip2/1.0.8-GCCcore-11.3.0/bin:/scicore/soft/apps/binutils/2.38-GCCcore-11.3.0/bin:/scicore/soft/apps/GCCcore/11.3.0/bin:/infai/sieverss/repos/bin:/infai/sieverss/local:/export/soft/lua_lmod/centos7/lmod/lmod/libexec:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:$PATH\nexport LD_LIBRARY_PATH=/scicore/soft/apps/libarchive/3.6.1-GCCcore-11.3.0/lib:/scicore/soft/apps/cURL/7.83.0-GCCcore-11.3.0/lib:/scicore/soft/apps/Python/3.10.4-GCCcore-11.3.0/lib:/scicore/soft/apps/OpenSSL/1.1/lib:/scicore/soft/apps/libffi/3.4.2-GCCcore-11.3.0/lib64:/scicore/soft/apps/GMP/6.2.1-GCCcore-11.3.0/lib:/scicore/soft/apps/XZ/5.2.5-GCCcore-11.3.0/lib:/scicore/soft/apps/SQLite/3.38.3-GCCcore-11.3.0/lib:/scicore/soft/apps/Tcl/8.6.12-GCCcore-11.3.0/lib:/scicore/soft/apps/libreadline/8.1.2-GCCcore-11.3.0/lib:/scicore/soft/apps/ncurses/6.3-GCCcore-11.3.0/lib:/scicore/soft/apps/bzip2/1.0.8-GCCcore-11.3.0/lib:/scicore/soft/apps/binutils/2.38-GCCcore-11.3.0/lib:/scicore/soft/apps/zlib/1.2.12-GCCcore-11.3.0/lib:/scicore/soft/apps/GCCcore/11.3.0/lib64')
    TIME_LIMIT = 1800
else:
    SUITE = [
        "miconic-strips:0-p01.pddl",
        "miconic-simpleadl-adl:0-s1-0.pddl",
    ]
    ENVIRONMENT = LocalEnvironment(processes=8)
    TIME_LIMIT = 15

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


def main():
    exp = Experiment(environment=ENVIRONMENT)
    exp.add_step("build", exp.build)
    exp.add_step("start", exp.start_runs)
    exp.add_step("parse", exp.parse)
    exp.add_fetcher(name="fetch", merge=False)

    exp.add_parser(get_parser())
    exp.add_resource("static_portfolios_image", IMAGE, symlink=True)
    exp.add_resource("ibacop_image", IBACOP_IMAGE, symlink=True)
    #exp.add_resource("run_plan", REPO / "plan.py")
    #exp.add_resource("fd_2018_configs", REPO / "configs/fd_2018_configs.py")
    exp.add_resource("filter_stderr", DIR / "filter-stderr.py")
    exp.add_resource("run_validate", "run-validate.sh")

    for portfolio_name in PORTFOLIOS:
        algorithm_name = portfolio_name
        portfolio = PORTFOLIOS_DIR / f"{portfolio_name}.py"
        for task in suites.build_suite(BENCHMARKS_DIR, SUITE):
            run = exp.add_run()
            run.add_resource("domain", task.domain_file, "domain.pddl")
            run.add_resource("problem", task.problem_file, "problem.pddl")
            # Use runlim to limit time and memory. It must be on the system PATH.
            internal_memory_limit = math.ceil(MEMORY_LIMIT * 0.9) # generously keep some memory for running the portfolio driver
            internal_time_limit = math.ceil(TIME_LIMIT * 0.99) # use a slightly smaller runtime limit for the portfolio
            run.add_command(
                "run-planner",
                [
                    "runlim",
                    "--output-file=runlim.txt",
                    f"--time-limit={TIME_LIMIT}",
                    f"--space-limit={MEMORY_LIMIT}",
                    "--propagate",
                    "{ibacop_image}" if "ibacop" in portfolio_name else "{static_portfolios_image}",
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


main()
