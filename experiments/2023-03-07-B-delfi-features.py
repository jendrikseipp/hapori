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
from collect_features_report import CollectFeaturesReport


sys.path.append(str(Path(__file__).parent.parent))
import plan


NODE = platform.node()
RUNNING_ON_CLUSTER = NODE.endswith((".scicore.unibas.ch", ".cluster.bc2.ch"))
DIR = Path(__file__).resolve().parent
REPO = DIR.parent
BENCHMARKS_DIR = REPO / "benchmarks"
assert BENCHMARKS_DIR.is_dir(), BENCHMARKS_DIR


MEMORY_LIMIT = 7800  # MiB
if RUNNING_ON_CLUSTER:
    SUITE = project.SUITE_STRIPS_AND_ADL
    ENVIRONMENT = BaselSlurmEnvironment(
        partition="infai_1",
        email="patrick.ferber@unibas.ch",
        memory_per_cpu="3500M",
        cpus_per_task=1,
        export=["PATH"],
        setup=BaselSlurmEnvironment.DEFAULT_SETUP,
        # Until recently, we had to load the Singularity module here
        # by adding "module load Singularity/2.6.1 2> /dev/null".
    )
    TIME_LIMIT = 1800
else:
    SUITE = ["gripper-strips:0-p01.pddl", "transport-strips:0-p01.pddl", "transport-strips:0-p20.pddl"]
    ENVIRONMENT = LocalEnvironment(processes=2)
    TIME_LIMIT = 120

exp = Experiment(environment=ENVIRONMENT)
exp.add_step("build", exp.build)
exp.add_step("start", exp.start_runs)
exp.add_fetcher(name="fetch")
# exp.add_parser(DIR / "singularity-parser.py")
DIR_FEATURE_EXTRACTOR = os.environ["DELFI_FEATURE_EXTRACTOR"]
exp.add_resource("feature_extractor", DIR_FEATURE_EXTRACTOR, symlink=True)

for task in suites.build_suite(BENCHMARKS_DIR, SUITE):
    run = exp.add_run()
    run.add_resource("domain", task.domain_file, "domain.pddl")
    run.add_resource("problem", task.problem_file, "problem.pddl")
    run.add_command(
        "extract_features",
        ["{feature_extractor}/image-only.py", "{domain}", "{problem}",
         "blank","--image-from-lifted-task"],
    )

    run.set_property("domain", task.domain)
    run.set_property("problem", task.problem)
    run.set_property("algorithm", "blubb")
    run.set_property("features", "delfi-lifted")
    run.set_property("id", ["delfi-lifted", task.domain, task.problem])


exp.add_report(CollectFeaturesReport(
    source_file="annotated-abstract-structure-graph.txt",
    directory_name="annotated_graphs",
    is_json=False),
    outfile="outfile1")

exp.add_report(CollectFeaturesReport(
    source_file="graph-gs-L-bolded-cs.png",
    directory_name="images",
    is_json=False),
    outfile="outfile2")
exp.run_steps()
