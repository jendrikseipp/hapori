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
REPO = DIR.parent
IMAGES_DIR = REPO / "images"
assert IMAGES_DIR.is_dir(), IMAGES_DIR
BENCHMARKS_DIR = REPO / "benchmarks"
assert BENCHMARKS_DIR.is_dir(), BENCHMARKS_DIR

MEMORY_LIMIT = 7800  # MiB
if RUNNING_ON_CLUSTER:
    SUITE = project.SUITE_STRIPS_AND_ADL
    ENVIRONMENT = BaselSlurmEnvironment(
        partition="infai_3",
        email="patrick.ferber@unibas.ch",
        memory_per_cpu="3940M",
        cpus_per_task=2,
        export=["PATH"],
        setup=BaselSlurmEnvironment.DEFAULT_SETUP,
        # Until recently, we had to load the Singularity module here
        # by adding "module load Singularity/2.6.1 2> /dev/null".
    )
else:
    SUITE = ["gripper-strips:0-p01.pddl", "transport-strips:0-p01.pddl", "transport-strips:0-p20.pddl"]
    ENVIRONMENT = LocalEnvironment(processes=2)

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



def get_image_name(file_name):
    parts = file_name.split("+")
    assert len(parts) == 2
    image_name = parts[1]
    if image_name.endswith(".py"):
        image_name = image_name[:-3]
    return image_name


def is_opt_config(image_name, config_name):
    image_name = image_name.lower()
    config_name = config_name.lower()
    for k , v in [("opt", True), ("sat", False), ("agl", False)]:
        if config_name.startswith(k) or config_name.endswith(k):
            return v
    if image_name == "ipc2018-fd-2018":
        return False
    if image_name == "ipc2018-lapkt-bfws":
        return False
    if image_name == "ipc2018-lapkt-dfs-plus":
        return False
    for k, v in [("opt", True), ("sat", False), ("agl", False)]:
        if image_name.find(f"-{k}-") > -1:
            return v
    assert False


def get_configs(name):
    planner = name.replace("-", "_")
    configs = plan.CONFIGS.get(name)
    if configs is None:
        yield "", name, [f"{{{planner}}}"]
    else:
        for config_name in configs:
            yield config_name, f"{name}+{config_name}", [f"{{{planner}}}", "--configs", config_name]


def construct(image_name):
    if image_name.endswith(".img"):
        image_name = image_name[:-4]

    exp = Experiment(environment=ENVIRONMENT)
    exp.add_step("build", exp.build)
    exp.add_step("start", exp.start_runs)
    exp.add_fetcher(name="fetch")
    exp.add_parser(DIR / "singularity-parser.py")

    file_image = IMAGES_DIR / f"{image_name}.img"
    assert file_image.is_file(), file_image
    exp.add_resource(image_name.replace("-", "_"), file_image, symlink=True)

    exp.add_resource("run_plan", DIR.parent / "plan.py")
    exp.add_resource("fd_2018_configs", DIR.parent / "fd_2018_configs.py")
    exp.add_resource("planners", DIR.parent / "planners", symlink=True)
    exp.add_resource("filter_stderr", DIR / "filter-stderr.py")

    for config_name, planner_name, config in get_configs(image_name):
        time_limit = 1800 if is_opt_config(image_name, config_name) else 300
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
                    time_limit,
                    "-V",
                    MEMORY_LIMIT,
                    "-w",
                    "watch.log",
                    "-v",
                    "values.log",
                    "{run_plan}"
                ] + config + [
                    "{domain}",
                    "{problem}",
                    "sas_plan",
                    "--not-check-subprocess",
                ],
            )
            # run.add_command("validate", ["validate", "{domain}", "{problem}", "sas_plan"])
            # Remove temporary files from old Fast Downward versions.
            run.add_command("rm-tmp-files", ["rm", "-f", "output.sas", "output"])
            run.add_command("filter-stderr", [sys.executable, "{filter_stderr}"])

            run.set_property("domain", task.domain)
            run.set_property("problem", task.problem)
            run.set_property("algorithm", [image_name, config_name])
            run.set_property("id", [planner_name, task.domain, task.problem])

    report = Path(exp.eval_dir) / f"{exp.name}.html"
    exp.add_report(BaseReport(attributes=ATTRIBUTES), outfile=report)
    # exp.add_parse_again_step()
    exp.run_steps()
