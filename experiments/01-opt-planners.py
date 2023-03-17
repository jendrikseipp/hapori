#! /usr/bin/env python

import csv
from pathlib import Path

from lab.experiment import Experiment

import project


DIR = Path(__file__).resolve().parent
REPO = project.get_repo_base()
BENCHMARKS_DIR = REPO / "benchmarks"
assert BENCHMARKS_DIR.exists(), BENCHMARKS_DIR
ATTRIBUTES = [
    "error",
    "coverage",
    "cost",
    "plan_length",
    "run_dir",
    "used_memory",
    "cpu_time",
]

exp = Experiment()
exp.add_step("remove-combined-properties", project.remove_properties, Path(exp.eval_dir))

domains_with_axioms = set()
domains_with_cond_effs = set()
with open("domain_properties.csv") as f:
    reader = csv.reader(f)
    next(reader)  # Skip header
    for row in reader:
        domain, axioms, cond_effs = row
        assert axioms in {"True", "False"}
        assert cond_effs in {"True", "False"}
        if axioms == "True":
            domains_with_axioms.add(domain)
        if cond_effs == "True":
            domains_with_cond_effs.add(domain)
print("Axioms:", sorted(domains_with_axioms))
print("Conditional effects:", sorted(domains_with_cond_effs))
print("Both:", sorted(domains_with_axioms & domains_with_cond_effs))

def strip_runs(run):
    for attr in ["node", "planner_wall_clock_time", "resident_memory", "run_dir", "unexplained_errors", "virtual_memory"]:
        if attr in run:
            del run[attr]
    image, config = run["algorithm"]
    config = config or "default"
    run["algorithm"] = f"{image}:{config}"
    run["image"] = image
    run["config"] = config
    if image == "ipc2018-opt-complementary1" or run["algorithm"] in {"ipc2018-symple1:symple100000AGL", "ipc2018-symple2:symple100000AGL"}:
        return False
    return run

for expname in [
    #"2023-03-02-A-complementary1",
    #"2023-03-02-B-complementary2",
    #"2023-03-02-C-decstar",
    #"2023-03-02-D-symple1",
    #"2023-03-02-E-symple2",
    #"2023-03-02-F-metis2",
    #"2023-03-02-G-delfi",
    #"2023-03-02-H-planningpdb",
    #"2023-03-02-I-scorpion",
    "2023-03-10+ipc2018-opt-complementary1",
    "2023-03-10+ipc2018-opt-complementary2",
    "2023-03-10+ipc2018-opt-delfi",
    "2023-03-10+ipc2018-opt-metis",
    "2023-03-10+ipc2018-opt-planning-pdbs",
    "2023-03-10+ipc2018-opt-scorpion",
    "2023-03-10+ipc2018-symple1",
    "2023-03-10+ipc2018-symple2",
    "2023-03-10+ipc2014-opt-symba1",

    #"2023-03-10+ipc2014-agl-jasper",
    #"2023-03-10+ipc2014-agl-mpc",
    #"2023-03-10+ipc2014-agl-probe",
    #"2023-03-10+ipc2018-agl-cerberus",
    #"2023-03-10+ipc2018-agl-freelunch-madagascar",
    #"2023-03-10+ipc2018-agl-mercury2014",
    #"2023-03-10+ipc2018-agl-merwin",
    #"2023-03-10+ipc2018-agl-olcff",
    #"2023-03-10+ipc2018-agl-saarplan",
    #"2023-03-10+ipc2018-fd-2018",
    #"2023-03-10+ipc2018-lapkt-dfs-plus",
    #"2023-03-10+ipc2018-lapkt-bfws",
    #"2023-03-10+ipc2018-saarplan",
]:
    project.fetch_algorithms(exp, expname)

for opt_config in [f"opt-config{i:02d}" for i in range(0, 7)]:
    project.fetch_algorithm(exp, "2023-03-10+ipc2018-decstar", ["ipc2018-decstar", opt_config])

project.add_absolute_report(exp, attributes=ATTRIBUTES, filter=[strip_runs], name=f"{exp.name}-full")

properties_hardest = Path(exp.eval_dir) / "properties-hardest.json"
exp.add_report(project.Hardest30Report(filter=[strip_runs]), outfile=properties_hardest, name="keep-only-30-hardest-tasks")
exp.add_step("compress-properties", project.compress, properties_hardest)
#project.add_absolute_report(exp, attributes=ATTRIBUTES, name=f"{exp.name}-hardest")

exp.run_steps()
