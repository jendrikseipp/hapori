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
    if run["algorithm"] in {"ipc2018-symple1:symple100000OPT", "ipc2018-symple2:symple100000OPT"}:
        return False
    return run

for expname in [
    "2023-03-10+ipc2014-agl-jasper",
    "2023-03-10+ipc2014-agl-mpc",
    "2023-03-10+ipc2014-agl-probe",
    "2023-03-10+ipc2018-agl-cerberus",
    "2023-03-10+ipc2018-agl-freelunch-madagascar",
    "2023-03-10+ipc2018-agl-mercury2014",
    "2023-03-10+ipc2018-agl-merwin",
    "2023-03-10+ipc2018-agl-olcff",
    "2023-03-10+ipc2018-agl-saarplan",
    "2023-03-10+ipc2018-fd-2018",
    "2023-03-10+ipc2018-lapkt-dfs-plus",
    "2023-03-10+ipc2018-lapkt-bfws",
    "2023-03-10+ipc2018-saarplan",
]:
    project.fetch_algorithms(exp, expname)

for config in ["symple100000AGL", "symple100000SAT"]:
    project.fetch_algorithm(exp, "2023-03-10+ipc2018-symple1", ["ipc2018-symple1", config])
    project.fetch_algorithm(exp, "2023-03-10+ipc2018-symple2", ["ipc2018-symple2", config])

project.add_absolute_report(exp, attributes=ATTRIBUTES, filter=[strip_runs], name=f"{exp.name}-full")

properties_hardest = Path(exp.eval_dir) / "properties-hardest.json"
exp.add_report(project.Hardest30Report(filter=[strip_runs]), outfile=properties_hardest, name="keep-only-30-hardest-tasks")
exp.add_step("compress-properties", project.compress, properties_hardest)
#project.add_absolute_report(exp, attributes=ATTRIBUTES, name=f"{exp.name}-hardest")

exp.run_steps()
