#! /usr/bin/env python

from collections import defaultdict
import csv
from pathlib import Path

from downward.reports import PlanningReport
from lab.experiment import Experiment
from lab.reports.filter import FilterReport

import project

exp = Experiment()
exp.add_step("remove-properties", project.remove_properties, Path(exp.eval_dir))

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
# print("Axioms:", sorted(domains_with_axioms))
# print("Conditional effects:", sorted(domains_with_cond_effs))
# print("Both:", sorted(domains_with_axioms & domains_with_cond_effs))

for expname in [
    '2023-11-27+ipc2014-agl-mpc-eval',
    '2023-11-27+ipc2014-agl-probe-eval',
    '2024-01-17-ipc2014-jasper-eval',
    '2023-11-27+ipc2014-opt-symba1-eval',
    '2024-01-19-ipc2018-cerberus-eval',
    '2023-11-27+ipc2018-decstar+opt-eval',
    '2023-11-27+ipc2018-decstar+sat-agl-eval',
    '2023-11-27+ipc2018-fd-2018+agl+A-eval',
    '2023-11-27+ipc2018-fd-2018+agl+B-eval',
    '2023-11-27+ipc2018-fd-2018+agl+C-eval',
    '2023-11-27+ipc2018-fd-2018+agl+D-eval',
    '2023-11-27+ipc2018-fd-2018+agl+E-eval',
    '2023-11-27+ipc2018-fd-2018+sat+A-eval',
    '2023-11-27+ipc2018-fd-2018+sat+B-eval',
    '2023-11-27+ipc2018-fd-2018+sat+C-eval',
    '2023-11-27+ipc2018-fd-2018+sat+D-eval',
    '2023-11-27+ipc2018-fd-2018+sat+E-eval',
    '2023-11-27+ipc2018-freelunch-madagascar-eval',
    '2023-11-27+ipc2018-lapkt-bfws-eval',
    '2023-11-27+ipc2018-lapkt-dfs-plus-eval',
    '2023-11-27+ipc2018-mercury2014-eval',
    '2023-11-27+ipc2018-merwin-eval',
    '2023-11-27+ipc2018-olcff-eval',
    '2023-11-27+ipc2018-opt-complementary2-eval',
    '2023-11-27+ipc2018-opt-delfi+A-eval',
    '2023-11-27+ipc2018-opt-delfi+B-eval',
    '2023-11-27+ipc2018-opt-metis-eval',
    '2023-11-27+ipc2018-opt-planning-pdbs-eval',
    '2023-11-27+ipc2018-opt-scorpion-eval',
    '2023-11-27+ipc2018-saarplan-eval',
    '2023-11-27+ipc2018-symple1-eval',
    '2023-11-27+ipc2018-symple2-eval',
]:
    exp.add_fetcher(
        f"data/{expname}",
        name=f"fetch-{expname}",
        merge=True
    )

HTML_ATTRIBUTES = [
    "cost",
    "coverage",
    "cpu_time",
    "error",
    "used_memory",
]

cavediving_adl_instances_val_does_not_like = [
    '1-p-2_2-2-0.4.pddl', '1-p-2_2-2-0.5.pddl', '1-p-2_2-2-0.6.pddl',
    '1-p-2_2-2-0.45.pddl', '1-p-2_2-2_2-0.4.pddl',
    '1-p-2_2-2_2-0.5.pddl', '1-p-2_2-2_2-0.6.pddl',
    '1-p-2_2-2_2-0.45.pddl', '1-p-2_2-2_2-0.55.pddl',
    '1-p-2_2_2-2-0.4.pddl', '1-p-2_2_2-2-0.5.pddl',
    '1-p-2_2_2-2-0.6.pddl', '1-p-2_2_2-2-0.45.pddl',
    '1-p-2_2_2-2-0.55.pddl', '1-p-2_2_2-2_2-0.4.pddl',
    '1-p-2_2_2-2_2-0.5.pddl', '1-p-2_2_2-2_2-0.6.pddl',
    '1-p-2_2_2-2_2-0.45.pddl', '1-p-2_2_2-2_2-0.55.pddl',
    '1-p-2_2_2-2_2_2-0.4.pddl', '1-p-2_2_2-2_2_2-0.5.pddl',
    '1-p-2_2_2-2_2_2-0.6.pddl', '1-p-2_2_2-2_2_2-0.45.pddl',
    '1-p-2_2_2-2_2_2-0.55.pddl', '1-p-2_2_2_2-2-0.4.pddl',
    '1-p-2_2_2_2-2-0.5.pddl', '1-p-2_2_2_2-2-0.6.pddl',
    '1-p-2_2_2_2-2-0.45.pddl', '1-p-2_2_2_2-2-0.55.pddl',
    '1-p-2_2_2_2-2_2-0.4.pddl', '1-p-2_2_2_2-2_2-0.5.pddl',
    '1-p-2_2_2_2-2_2-0.6.pddl', '1-p-2_2_2_2-2_2-0.45.pddl',
    '1-p-2_2_2_2-2_2-0.55.pddl', '1-p-3_2-2-0.5.pddl',
    '1-p-3_2-2-0.6.pddl', '1-p-3_2-2-0.55.pddl',
    '1-p-3_2-3_2-0.4.pddl', '1-p-3_2-3_2-0.5.pddl',
    '1-p-3_2-3_2-0.6.pddl', '1-p-3_2-3_2-0.45.pddl',
    '1-p-3_2-3_2-0.55.pddl', '1-p-3_2_2-2-0.4.pddl',
    '1-p-3_2_2-2-0.5.pddl', '1-p-3_2_2-2-0.6.pddl',
    '1-p-3_2_2-2-0.45.pddl', '1-p-3_2_2-2-0.55.pddl',
    '1-p-3_2_2-2_2-0.4.pddl', '1-p-3_2_2-2_2-0.5.pddl',
    '1-p-3_2_2-2_2-0.6.pddl', '1-p-3_2_2-2_2-0.45.pddl',
    '1-p-3_2_2-2_2-0.55.pddl', '1-p-3_2_2-3_2_2-0.4.pddl',
    '1-p-3_2_2-3_2_2-0.5.pddl', '1-p-3_2_2-3_2_2-0.6.pddl',
    '1-p-3_2_2-3_2_2-0.45.pddl', '1-p-3_2_2-3_2_2-0.55.pddl',
    '1-p-3_3-3-0.4.pddl', '1-p-3_3-3-0.5.pddl', '1-p-3_3-3-0.6.pddl',
    '1-p-3_3-3-0.45.pddl', '1-p-3_3-3-0.55.pddl',
    '1-p-3_3-3_3-0.4.pddl', '1-p-3_3-3_3-0.5.pddl',
    '1-p-3_3-3_3-0.6.pddl', '1-p-3_3-3_3-0.45.pddl',
    '1-p-3_3-3_3-0.55.pddl', '1-p-3_3_2-2-0.4.pddl',
    '1-p-3_3_2-2-0.5.pddl', '1-p-3_3_2-2-0.6.pddl',
    '1-p-3_3_2-2-0.45.pddl', '1-p-3_3_2-2-0.55.pddl',
    '1-p-3_3_2-3_2-0.4.pddl', '1-p-3_3_2-3_2-0.5.pddl',
    '1-p-3_3_2-3_2-0.6.pddl', '1-p-3_3_2-3_2-0.45.pddl',
    '1-p-3_3_2-3_2-0.55.pddl', '1-p-3_3_2-3_3_2-0.4.pddl',
    '1-p-3_3_2-3_3_2-0.5.pddl', '1-p-3_3_2-3_3_2-0.6.pddl',
    '1-p-3_3_2-3_3_2-0.45.pddl', '1-p-3_3_2-3_3_2-0.55.pddl',
    '1-p-3_3_3-3-0.4.pddl', '1-p-3_3_3-3-0.5.pddl',
    '1-p-3_3_3-3-0.6.pddl', '1-p-3_3_3-3-0.45.pddl',
    '1-p-3_3_3-3-0.55.pddl', '1-p-3_3_3-3_3-0.4.pddl',
    '1-p-3_3_3-3_3-0.5.pddl', '1-p-3_3_3-3_3-0.6.pddl',
    '1-p-3_3_3-3_3-0.45.pddl', '1-p-3_3_3-3_3-0.55.pddl',
    '1-p-3_3_3-3_3_3-0.4.pddl', '1-p-3_3_3-3_3_3-0.5.pddl',
    '1-p-3_3_3-3_3_3-0.6.pddl', '1-p-3_3_3-3_3_3-0.45.pddl',
    '1-p-3_3_3-3_3_3-0.55.pddl',
]
def filter_instances_which_val_cannot_handle(run):
    if (run['domain'] == 'cavediving-adl' and run['problem'] in cavediving_adl_instances_val_does_not_like):
        return False
    return True

"""
Check for inconsistencies in the training data.

TODOs:
- exclude planners from entire domains if they don't support the PDDL
fragment or if they produce invalid plans.
- for some outcomes of type crash (e.g., symba, mpc), it is pretty
clear that they crash because they don't support the PDDL fragment.
The outcome should be changed in that case (or the parser adapted
when running it again).
- if using a different validator than VAL, check if the above
cavediving-adl instances are parsed correctly and if so, stop excluding
them.
"""
class VerifyDataReport(PlanningReport):
    def get_text(self):
        assert len(self.algorithms) == 191
        algo_domain_tasks_with_invalid_plan_val = defaultdict(lambda: defaultdict(list))
        algo_domain_tasks_with_invalid_plan_upv = defaultdict(lambda: defaultdict(list))
        algo_domain_tasks_with_val_plan_too_long = defaultdict(lambda: defaultdict(list))
        algo_domain_tasks_with_val_problem = defaultdict(lambda: defaultdict(list))
        algo_domain_tasks_with_upv_problem = defaultdict(lambda: defaultdict(list))
        for run in self.props.values():
            """
            the if below currently triggers on domains with non-unit cost because UPV seemingly ignores
            missing "minimize total-cost metric" statements in problem files (which should probably not
            be missing in the first place...)
            """
            # if "val_cost" in run and run["upv_cost"] is not None and run["val_cost"] != run["upv_cost"]:
                # print("val and upv disagree")
                # print(run)
            if run["val_invalid_plan"]:
                algo_domain_tasks_with_invalid_plan_val[run["algorithm"]][run["domain"]].append((run["problem"], run["run_dir"]))
            if run["upv_invalid_plan"]:
                algo_domain_tasks_with_invalid_plan_upv[run["algorithm"]][run["domain"]].append((run["problem"], run["run_dir"]))
            if run["val_plan_too_long"]:
                algo_domain_tasks_with_val_plan_too_long[run["algorithm"]][run["domain"]].append((run["problem"], run["run_dir"]))
                """
                the if below only triggers for one task of termes-strips, maybe upv cannot deal with negative preconditions?
                /infai/sieverss/repos/hapori/experiments/data/2023-11-27+ipc2018-fd-2018+sat+E/runs-43001-43100/43074
                validate.bin domain.pddl problem.pddl sas_plan
                terminate called after throwing an instance of 'parser::pddl::UnknownToken'
                  what():  NOT does not name a known token
                Line 156, column 6: Aborted
                """
                if not run["coverage"] and not run["invalid_plan"]:
                    print("val cannot handle plan; upv apparently neither")
                    print(run)
            if run["plan_files"] and "val_cost" not in run and not run["val_invalid_plan"] and not run["val_plan_too_long"]:
                algo_domain_tasks_with_val_problem[run["algorithm"]][run["domain"]].append((run["problem"], run["run_dir"]))
            if run["plan_files"] and run["upv_cost"] is None and not run["upv_invalid_plan"]:
                algo_domain_tasks_with_upv_problem[run["algorithm"]][run["domain"]].append((run["problem"], run["run_dir"]))
        lines = []
        lines.append("algos on problems with found plan files but no recorded coverage and no invalid plan according to val, minus those for which we know that the plan files are too long for val")
        for algo, domain_tasks in algo_domain_tasks_with_val_problem.items():
            lines.append(f"{algo}:")
            for domain, tasks in domain_tasks.items():
                lines.append(f"    {domain}:")
                line = [" ".join(task) for task in tasks]
                lines.append("        " + " ".join(line))
        lines.append("algos on problems with found plan files but no recorded coverage and no invalid plan according to upv")
        for algo, domain_tasks in algo_domain_tasks_with_upv_problem.items():
            lines.append(f"{algo}:")
            for domain, tasks in domain_tasks.items():
                lines.append(f"    {domain}:")
                line = [" ".join(task) for task in tasks]
                lines.append("        " + " ".join(line))
        lines.append("algos on problems with too long val plan")
        for algo, domain_tasks in algo_domain_tasks_with_val_plan_too_long.items():
            lines.append(f"{algo}:")
            for domain, tasks in domain_tasks.items():
                lines.append(f"    {domain}:")
                line = [" ".join(task) for task in tasks]
                lines.append("        " + " ".join(line))
        lines.append("algos on problems with val-invalid plans")
        for algo, domain_tasks in algo_domain_tasks_with_invalid_plan_val.items():
            lines.append(f"{algo}:")
            for domain, tasks in domain_tasks.items():
                lines.append(f"    {domain}:")
                line = [" ".join(task) for task in tasks]
                lines.append("        " + " ".join(line))
        lines.append("algos on problems with upv-invalid plans")
        for algo, domain_tasks in algo_domain_tasks_with_invalid_plan_upv.items():
            lines.append(f"{algo}:")
            for domain, tasks in domain_tasks.items():
                lines.append(f"    {domain}:")
                line = [" ".join(task) for task in tasks]
                lines.append("        " + " ".join(line))
        return "\n".join(lines)

exp.add_report(
    VerifyDataReport(
            attributes=HTML_ATTRIBUTES,
            filter=[filter_instances_which_val_cannot_handle],
            format="txt",
        ),
        name=f"{exp.name}-verify")
exp.add_step("compress-properties", project.compress, Path(exp.eval_dir) / "properties")

class ProcessRuns:
    def __init__(self, track):
        self.track = track

    def __call__(self, run):
        if run["algorithm"] == "opt+ipc2018-decstar+opt-config01":
            # this algorithm found suboptimal solutions on instances
            # 0-p02.pddl and 0-p22.pddl of barman-strips.
            return False
        track, planner, config = run["algorithm"].split('+')
        if self.track != track:
            return False
        run["planner"] = planner
        run["config"] = config
        return run

for track in ["opt", "sat", "agl"]:
    process_runs = ProcessRuns(track)
    quality_filter = project.QualityFilters()
    properties_full = Path(exp.eval_dir) / f"properties-full-{track}.json"
    exp.add_report(
        FilterReport(filter=[filter_instances_which_val_cannot_handle,quality_filter.store_costs,quality_filter.add_quality,process_runs]),
        outfile=properties_full,
        name=f"properties-full-{track}")
    exp.add_step(f"compress-properties-full-{track}", project.compress, properties_full)

for track in ["opt", "sat", "agl"]:
    process_runs = ProcessRuns(track)
    quality_filter = project.QualityFilters()
    properties_hardest = Path(exp.eval_dir) / f"properties-hardest-{track}.json"
    exp.add_report(
        project.Hardest30Report(filter=[filter_instances_which_val_cannot_handle,quality_filter.store_costs,quality_filter.add_quality,process_runs]),
        outfile=properties_hardest,
        name=f"properties-hardest-{track}")
    exp.add_step(f"compress-properties-hardest-{track}", project.compress, properties_hardest)

exp.run_steps()
