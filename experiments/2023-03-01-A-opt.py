#! /usr/bin/env python

"""
Example experiment for running Singularity planner images.

The time and memory limits set with Lab can be circumvented by solvers
that fork child processes. Their resource usage is not checked. If you're
running solvers that don't check their resource usage like Fast Downward,
we recommend using cgroups or the "runsolver" tool to enforce resource
limits. Since setting time limits for solvers with cgroups is difficult,
the experiment below uses the "runsolver" tool, which has been used in
multiple SAT competitions to enforce resource limits. For the experiment
to run, the runsolver binary needs to be on the PATH. You can obtain a
runsolver copy from https://github.com/jendrikseipp/runsolver.

A note on running Singularity on clusters: reading large Singularity files
over the network is not optimal, so we recommend copying the images to a
local filesystem (e.g., /tmp/) before running experiments.
"""

import os
from pathlib import Path
import platform
import sys

from downward import suites
from downward.reports.absolute import AbsoluteReport
from lab.environments import BaselSlurmEnvironment, LocalEnvironment
from lab.experiment import Experiment


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
REPO = DIR.parent
IMAGES_DIR = REPO / "images"
assert IMAGES_DIR.is_dir(), IMAGES_DIR
BENCHMARKS_DIR = REPO / "benchmarks"
assert BENCHMARKS_DIR.is_dir(), BENCHMARKS_DIR

MEMORY_LIMIT = 6144  # MiB
if RUNNING_ON_CLUSTER:
    SUITE = project.SUITE_STRIPS_AND_ADL
    ENVIRONMENT = BaselSlurmEnvironment(
        partition="infai_2",
        email="patrick.ferber@unibas.ch",
        memory_per_cpu="6350M",
        export=["PATH"],
        setup=BaselSlurmEnvironment.DEFAULT_SETUP,
        # Until recently, we had to load the Singularity module here
        # by adding "module load Singularity/2.6.1 2> /dev/null".
    )
    TIME_LIMIT = 1800
else:
    SUITE = ["gripper-strips:0-p01.pddl", "transport-strips:0-p01.pddl", "transport-strips:0-p20.pddl"]
    ENVIRONMENT = LocalEnvironment(processes=2)
    TIME_LIMIT = 5

ATTRIBUTES = [
    "cost",
    "plan_length",
    "coverage",
    "error",
    "run_dir",
    "cpu_time",
    "wall_time",
    "used_memory",
    "resident_memory",
    "virtual_memory",
]

exp = Experiment(environment=ENVIRONMENT)
exp.add_step("build", exp.build)
exp.add_step("start", exp.start_runs)
exp.add_fetcher(name="fetch")
exp.add_parser(DIR / "singularity-parser.py")


def get_image(name):
    planner = name.replace("-", "_")
    image = IMAGES_DIR / (name + ".img")
    assert image.is_file(), image
    return planner, image

RAW_IMAGES = ['ipc2014-opt-symba1'] if not RUNNING_ON_CLUSTER else ['ipc2014-opt-symba1', 'ipc2018-opt-complementary2', 'ipc2018-opt-decstar', 'ipc2018-opt-delfi-blind', 'ipc2018-opt-delfi-celmcut', 'ipc2018-opt-delfi-ipdb-60s', 'ipc2018-opt-delfi-ipdb', 'ipc2018-opt-delfi-mas-miasm', 'ipc2018-opt-delfi-mas-sccdfp-60s', 'ipc2018-opt-delfi-mas-sccdfp', 'ipc2018-opt-fdms1', 'ipc2018-opt-fdms2', 'ipc2018-opt-metis1', 'ipc2018-opt-metis2', 'ipc2018-opt-planning-pdbs', 'ipc2018-opt-scorpion', 'ipc2018-opt-scorpion-nodiv', 'ipc2018-opt-symple1', 'ipc2018-opt-symple2']
IMAGES = [get_image(image) for image in RAW_IMAGES]

for planner, image in IMAGES:
    exp.add_resource(planner, image, symlink=True)

exp.add_resource("run_singularity", DIR / "run-singularity.sh")
exp.add_resource("filter_stderr", DIR / "filter-stderr.py")

for planner, _ in IMAGES:
    for task in suites.build_suite(BENCHMARKS_DIR, SUITE):
        run = exp.add_run()
        run.add_resource("domain", task.domain_file, "domain.pddl")
        run.add_resource("problem", task.problem_file, "problem.pddl")
        # Use runsolver to limit time and memory. It must be on the system
        # PATH. Important: we cannot use time_limit and memory_limit of
        # Lab's add_command() because setting the same memory limit with
        # runsolver again using setrlimit fails.
        run.add_command(
            "run-planner",
            [
                "runsolver",
                "-C",
                TIME_LIMIT,
                "-V",
                MEMORY_LIMIT,
                "-w",
                "watch.log",
                "-v",
                "values.log",
                "{run_singularity}",
                f"{{{planner}}}",
                "{domain}",
                "{problem}",
                "sas_plan",
            ],
        )
        # Remove temporary files from old Fast Downward versions.
        run.add_command("rm-tmp-files", ["rm", "-f", "output.sas", "output"])
        run.add_command("filter-stderr", [sys.executable, "{filter_stderr}"])

        run.set_property("domain", task.domain)
        run.set_property("problem", task.problem)
        run.set_property("algorithm", planner)
        run.set_property("id", [planner, task.domain, task.problem])

report = Path(exp.eval_dir) / f"{exp.name}.html"
exp.add_report(BaseReport(attributes=ATTRIBUTES), outfile=report)

exp.run_steps()
