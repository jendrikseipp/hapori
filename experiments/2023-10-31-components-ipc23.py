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

import os
from pathlib import Path
import platform
import sys

from singularity_parser import get_parser

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

MEMORY_LIMIT = 3500  # MiB
if RUNNING_ON_CLUSTER:
    SUITE = project.SUITE_IPC23
    ENVIRONMENT = BaselSlurmEnvironment(
        partition="infai_3",
        email="silvan.sievers@unibas.ch",
        memory_per_cpu="4028M",
        cpus_per_task=1,
        # paths obtained via:
        # $ module purge
        # $ module -q load Python/3.10.4-GCCcore-11.3.0
        # $ module -q load GCC/11.3.0
        # $ module -q load CMake/3.23.1-GCCcore-11.3.0
        # $ echo $PATH
        # $ echo $LD_LIBRARY_PATH
        setup='export PATH=/scicore/soft/apps/CMake/3.23.1-GCCcore-11.3.0/bin:/scicore/soft/apps/libarchive/3.6.1-GCCcore-11.3.0/bin:/scicore/soft/apps/cURL/7.83.0-GCCcore-11.3.0/bin:/scicore/soft/apps/Python/3.10.4-GCCcore-11.3.0/bin:/scicore/soft/apps/OpenSSL/1.1/bin:/scicore/soft/apps/XZ/5.2.5-GCCcore-11.3.0/bin:/scicore/soft/apps/SQLite/3.38.3-GCCcore-11.3.0/bin:/scicore/soft/apps/Tcl/8.6.12-GCCcore-11.3.0/bin:/scicore/soft/apps/ncurses/6.3-GCCcore-11.3.0/bin:/scicore/soft/apps/bzip2/1.0.8-GCCcore-11.3.0/bin:/scicore/soft/apps/binutils/2.38-GCCcore-11.3.0/bin:/scicore/soft/apps/GCCcore/11.3.0/bin:/infai/sieverss/repos/bin:/infai/sieverss/local:/export/soft/lua_lmod/centos7/lmod/lmod/libexec:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:$PATH\nexport LD_LIBRARY_PATH=/scicore/soft/apps/libarchive/3.6.1-GCCcore-11.3.0/lib:/scicore/soft/apps/cURL/7.83.0-GCCcore-11.3.0/lib:/scicore/soft/apps/Python/3.10.4-GCCcore-11.3.0/lib:/scicore/soft/apps/OpenSSL/1.1/lib:/scicore/soft/apps/libffi/3.4.2-GCCcore-11.3.0/lib64:/scicore/soft/apps/GMP/6.2.1-GCCcore-11.3.0/lib:/scicore/soft/apps/XZ/5.2.5-GCCcore-11.3.0/lib:/scicore/soft/apps/SQLite/3.38.3-GCCcore-11.3.0/lib:/scicore/soft/apps/Tcl/8.6.12-GCCcore-11.3.0/lib:/scicore/soft/apps/libreadline/8.1.2-GCCcore-11.3.0/lib:/scicore/soft/apps/ncurses/6.3-GCCcore-11.3.0/lib:/scicore/soft/apps/bzip2/1.0.8-GCCcore-11.3.0/lib:/scicore/soft/apps/binutils/2.38-GCCcore-11.3.0/lib:/scicore/soft/apps/zlib/1.2.12-GCCcore-11.3.0/lib:/scicore/soft/apps/GCCcore/11.3.0/lib64')
    OPT_TIME_LIMIT = 1800
    SAT_TIME_LIMIT = 300
else:
    # SUITE = ["labyrinth-opt23-adl:p01.pddl", "recharging-robots-opt23-adl:p01.pddl"]
    SUITE = ["miconic-strips:0-p01.pddl"]
    ENVIRONMENT = LocalEnvironment(processes=4)
    OPT_TIME_LIMIT = 1
    SAT_TIME_LIMIT = 1

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
    "invalid_plan",
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
    for k, v in [("opt", True), ("sat", False), ("agl", False)]:
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
    configs = plan.CONFIGS.get(name)
    assert configs is not None
    for config_name in configs:
        yield config_name, f"{name}+{config_name}"


def main(image_name):
    exp = Experiment(environment=ENVIRONMENT)
    exp.add_step("build", exp.build)
    exp.add_step("start", exp.start_runs)
    exp.add_step("parse", exp.parse)
    exp.add_fetcher(name="fetch")
    exp.add_parser(get_parser())

    file_image = IMAGES_DIR / image_name
    assert file_image.is_file(), file_image
    exp.add_resource("image", file_image, symlink=True)

    exp.add_resource("run_plan", DIR.parent / "plan.py")
    exp.add_resource("fd_2018_configs", DIR.parent / "configs/fd_2018_configs.py")
    exp.add_resource("filter_stderr", DIR / "filter-stderr.py")
    exp.add_resource("run_validate", "run-validate.sh")

    for planner_nick in list(plan.CONFIGS.keys()):
        for config_name, planner_name in get_configs(planner_nick):
            time_limit = OPT_TIME_LIMIT if is_opt_config(planner_nick, config_name) else SAT_TIME_LIMIT
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
                        "--cpu-limit",
                        time_limit,
                        "--rss-swap-limit",
                        MEMORY_LIMIT,
                        "--watcher-data",
                        "watch.log",
                        "--var",
                        "values.log",
                        "{image}",
                        "{domain}",
                        "{problem}",
                        "sas_plan",
                        planner_nick,
                        config_name
                    ],
                )
                run.add_command("run-validate", ["{run_validate}", "{domain}", "{problem}", "sas_plan"])
                # Remove temporary files from old Fast Downward versions.
                run.add_command("rm-tmp-files", ["rm", "-f", "output.sas", "output"])
                run.add_command("filter-stderr", [sys.executable, "{filter_stderr}"])

                run.set_property("domain", task.domain)
                run.set_property("problem", task.problem)
                run.set_property("algorithm", planner_name)
                run.set_property("id", [planner_name, task.domain, task.problem])

    report = Path(exp.eval_dir) / f"{exp.name}.html"
    exp.add_report(BaseReport(attributes=ATTRIBUTES), outfile=report)
    exp.run_steps()

if __name__ == "__main__":
    main("hapori_components.img")
