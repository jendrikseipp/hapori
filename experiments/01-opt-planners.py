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
    if "unexplained_errors" in run:
        del run["unexplained_errors"]
    return run

for expname in [
    "2023-03-02-A-complementary1",
    "2023-03-02-B-complementary2",
    "2023-03-02-C-decstar",
    "2023-03-02-D-symple1",
    "2023-03-02-E-symple2",
    "2023-03-02-F-metis2",
    "2023-03-02-G-delfi",
    "2023-03-02-H-planningpdb",
    "2023-03-02-I-scorpion",
]:
    project.fetch_algorithms(exp, expname)

#project.add_absolute_report(exp, attributes=ATTRIBUTES, name=f"{exp.name}-full")

exp.add_report(project.Hardest30Report(), outfile="properties", name="keep-only-hardest-tasks-in-properties")
exp.add_step("compress-properties", project.compress_properties, Path(exp.eval_dir))

project.add_absolute_report(exp, attributes=ATTRIBUTES, filter=[strip_runs], name=f"{exp.name}-hardest")

exp.run_steps()
