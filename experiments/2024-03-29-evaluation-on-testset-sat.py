#! /usr/bin/env python3

import math
from pathlib import Path
import platform
import subprocess
import sys

from portfolio_parser_runlim import get_parser

from downward import suites
from downward.reports.absolute import AbsoluteReport
from lab.environments import BaselSlurmEnvironment, LocalEnvironment, TetralithEnvironment
from lab.experiment import Experiment

import project


DIR = Path(__file__).resolve().parent
REPO = project.get_repo_base()
IMAGES = {
    "static_sequential_portfolios": REPO / "2024-02-09-hapori_sequential_portfolios.sif",
    "delfi": REPO / "2024-04-01-hapori_delfi.sif",
    "ibacop": REPO / "2024-03-29-hapori_ibacop2.sif",
}
for image in IMAGES.values():
    assert image.is_file(), image
BENCHMARKS_DIR = REPO / "benchmarks"
assert BENCHMARKS_DIR.is_dir(), BENCHMARKS_DIR
SEQUENTIAL_PORTFOLIOS_DIR = REPO / "sequential-portfolios"
SEQUENTIAL_PORTFOLIOS = [
    "cluster-sat",
    "domain-wise-sat",
    "greedy-sat",
    "ibacop2-sat",
    "increasing-time-limit-sat",
    "miplan-hardest-sat",
    "randomized-iterative-search-sat",
    "stonesoup-sat",
    "uniform-sat",
]
ALGORITHM_SELECTORS = [
    "delfi-satisficing",
]
MEMORY_LIMIT = 8000  # MiB
#SUITE = project.SUITE_IPC23_SAT
SUITE = [
    f"{domain}:p01.pddl" for domain in project.SUITE_IPC23_SAT
] + [
    f"{domain}:p20.pddl" for domain in project.SUITE_IPC23_SAT
]
if project.REMOTE:
    ENVIRONMENT = BaselSlurmEnvironment(
        partition="infai_3",
        email="jendrik.seipp@liu.se",
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
    #ENVIRONMENT = TetralithEnvironment(
    #    email="jendrik.seipp@liu.se",
    #    memory_per_cpu="9G",
    #    extra_options="#SBATCH --account=naiss2023-5-314",
    #)
    TIME_LIMIT = 1800
else:
    ENVIRONMENT = LocalEnvironment(processes=12)
    TIME_LIMIT = 10


def main():
    exp = Experiment(environment=ENVIRONMENT)
    exp.add_step("build", exp.build)
    exp.add_step("start", exp.start_runs)
    exp.add_step("parse", exp.parse)
    exp.add_fetcher(name="fetch", merge=False)

    exp.add_parser(get_parser())
    for name, image in IMAGES.items():
        exp.add_resource(name, image, symlink=True)
    exp.add_resource("filter_stderr", DIR / "filter-stderr.py")
    exp.add_resource("run_validate", "run-validate.sh")

    for task in suites.build_suite(BENCHMARKS_DIR, SUITE):
        for algorithm_name in SEQUENTIAL_PORTFOLIOS + ALGORITHM_SELECTORS:
            run = exp.add_run()
            run.add_resource("domain", task.domain_file, "domain.pddl")
            run.add_resource("problem", task.problem_file, "problem.pddl")
            if "delfi" in algorithm_name:
                image = "{delfi}"
            elif "ibacop" in algorithm_name:
                image = "{ibacop}"
            else:
                image = "{static_sequential_portfolios}"
            # Use runlim to limit time and memory. It must be on the system PATH.
            runlim_cmd = [
                "runlim",
                "--output-file=runlim.txt",
                f"--time-limit={TIME_LIMIT}",
                f"--space-limit={MEMORY_LIMIT}",
                "--propagate",
                image,
                "{domain}",
                "{problem}",
                "sas_plan",
            ]
            if algorithm_name == "delfi-satisficing":
                runlim_cmd.append("hardestsatisficing")
            else:
                internal_memory_limit = math.ceil(MEMORY_LIMIT * 0.9)  # generously keep some memory for running the portfolio driver
                internal_time_limit = math.ceil(TIME_LIMIT * 0.99)  # use a slightly smaller runtime limit for the portfolio
                runlim_cmd.extend([
                    str(internal_memory_limit),
                    str(internal_time_limit),
                    str(SEQUENTIAL_PORTFOLIOS_DIR / f"hapori-{algorithm_name}.py"),
                ])

            run.add_command("run-planner", runlim_cmd)
            run.add_command("run-validate", ["{run_validate}", "{domain}", "{problem}", "sas_plan"])
            # Remove temporary files from old Fast Downward versions.
            run.add_command("rm-tmp-files", ["rm", "-f", "output.sas", "output"])
            run.add_command("filter-stderr", [sys.executable, "{filter_stderr}"])

            run.set_property("domain", task.domain)
            run.set_property("problem", task.problem)
            run.set_property("algorithm", algorithm_name)
            run.set_property("id", [algorithm_name, task.domain, task.problem])

    project.add_default_report_steps(exp)
    exp.run_steps()


main()
