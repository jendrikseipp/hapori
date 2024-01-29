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
unsolvable_instances = [
    'mystery-strips:0-prob04.pddl',
    'mystery-strips:0-prob05.pddl',
    'mystery-strips:0-prob07.pddl',
    'mystery-strips:0-prob08.pddl',
    'mystery-strips:0-prob12.pddl',
    'mystery-strips:0-prob16.pddl',
    'mystery-strips:0-prob18.pddl',
    'mystery-strips:0-prob21.pddl',
    'mystery-strips:0-prob22.pddl',
    'mystery-strips:0-prob23.pddl',
    'mystery-strips:0-prob24.pddl',
    'no-mystery-strips:0-prob04.pddl',
    'no-mystery-strips:0-prob05.pddl',
    'no-mystery-strips:0-prob07.pddl',
    'no-mystery-strips:0-prob08.pddl',
    'no-mystery-strips:0-prob12.pddl',
    'no-mystery-strips:0-prob16.pddl',
    'no-mystery-strips:0-prob18.pddl',
    'no-mystery-strips:0-prob21.pddl',
    'no-mystery-strips:0-prob22.pddl',
    'no-mystery-strips:0-prob23.pddl',
    'no-mystery-strips:0-prob24.pddl',
]

def filter_instances(run):
    if (run['domain'] == 'cavediving-adl' and run['problem'] in cavediving_adl_instances_val_does_not_like):
        return False
    if f"{run['domain']}:{run['problem']}" in unsolvable_instances:
        return False
    return True

invalid_plans = {
    'agl+ipc2014-jasper+default': ['optical-telegraphs-adl', 'philosophers-adl', 'settlers-adl'],
    'agl+ipc2018-decstar+agl-config02': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config00': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config01': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config02': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config03': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config04': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config05': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config06': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config07': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config08': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config09': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config10': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config11': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config14': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config15': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config16': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config18': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config19': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config21': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config22': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config23': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config24': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config25': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config27': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config28': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config29': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config30': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config31': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config34': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config36': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config37': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config38': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config39': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config40': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config41': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config43': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config44': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config46': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config48': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config50': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config51': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config52': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config54': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config55': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config59': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config60': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-fd-2018+config61': ['optical-telegraphs-adl', 'philosophers-adl'],
    'agl+ipc2018-lapkt-bfws+bfws-pref-agl': ['airport-adl', 'assembly-adl', 'openstacks-adl', 'trucks-adl'],
    'agl+ipc2018-lapkt-bfws+dual-bfws-agl': ['airport-adl', 'assembly-adl', 'citycar-adl', 'openstacks-adl', 'trucks-adl'],
    'agl+ipc2018-lapkt-bfws+poly-bfws': ['airport-adl', 'assembly-adl', 'openstacks-adl', 'trucks-adl'],
    'agl+ipc2018-lapkt-dfs-plus+default': ['airport-adl', 'assembly-adl', 'openstacks-adl', 'trucks-adl'],
    'opt+ipc2018-decstar+opt-config06': ['optical-telegraphs-adl', 'philosophers-adl'],
    'opt+ipc2018-opt-complementary2+default': ['hanoi-strips'],
    'opt+ipc2018-opt-delfi+h2-simpless-dks-900masginfsccdfp': ['hanoi-strips'],
    'opt+ipc2018-opt-delfi+h2-simpless-dks-blind': ['hanoi-strips'],
    'opt+ipc2018-opt-delfi+h2-simpless-dks-zopdbsgenetic': ['hanoi-strips'],
    'opt+ipc2018-opt-delfi+h2-simpless-oss-blind': ['hanoi-strips'],
    'opt+ipc2018-opt-delfi+h2-simpless-oss-cpdbshc900': ['hanoi-strips'],
    'opt+ipc2018-opt-delfi+h2-simpless-oss-masginfsccdfp': ['hanoi-strips'],
    'opt+ipc2018-opt-delfi+h2-simpless-oss-zopdbsgenetic': ['hanoi-strips'],
    'opt+ipc2018-opt-delfi+simpless-dks-masb50kmiasmdfp': ['hanoi-strips'],
    'opt+ipc2018-opt-delfi+simpless-oss-masb50kmiasmdfp': ['hanoi-strips'],
    'opt+ipc2018-opt-scorpion+default': ['hanoi-strips'],
    'sat+ipc2014-jasper+default': ['optical-telegraphs-adl', 'philosophers-adl', 'settlers-adl'],
    'sat+ipc2018-decstar+sat-config02': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-decstar+sat-config03': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config00': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config01': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config02': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config03': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config04': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config05': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config06': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config07': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config08': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config09': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config10': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config11': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config14': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config15': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config16': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config18': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config19': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config21': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config22': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config23': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config24': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config25': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config27': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config28': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config29': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config30': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config31': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config34': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config36': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config37': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config38': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config39': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config40': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config41': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config43': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config44': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config46': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config48': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config50': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config51': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config52': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config54': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config55': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config59': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config60': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-fd-2018+config61': ['optical-telegraphs-adl', 'philosophers-adl'],
    'sat+ipc2018-lapkt-bfws+bfws-pref-sat': ['airport-adl', 'assembly-adl', 'openstacks-adl'],
    'sat+ipc2018-lapkt-bfws+dual-bfws-sat': ['airport-adl', 'assembly-adl', 'openstacks-adl', 'trucks-adl'],
    'sat+ipc2018-lapkt-bfws+poly-bfws': ['airport-adl', 'assembly-adl', 'openstacks-adl', 'trucks-adl'],
    'sat+ipc2018-lapkt-dfs-plus+default': ['airport-adl', 'assembly-adl', 'openstacks-adl', 'trucks-adl'],
}

def exclude_planners_from_domains(run):
    """
    - exclude all planners from a domain if any of their plans for that
      domain are invalid
    - exclude opt+ipc2018-decstar+opt-config01 from barman-strips
      because it found suboptimal solutions on instances 0-p02.pddl and
      0-p22.pddl.
    """
    if ((run["algorithm"] in invalid_plans and
        run["domain"] in invalid_plans[run["algorithm"]]) or
        (run["algorithm"] == "opt+ipc2018-decstar+opt-config01" and
        run["domain"] == "barman-strips")):
        run["cost"] = None
        run["coverage"] = 0
        run["invalid_plan"] = True
        run["error"] = "invalid_plan"
        run["cpu_time"] = None
        run["wall_time"] = None
    return run

"""
Check for inconsistencies in the training data.

TODOs:
- exclude planners from entire domains if they don't support the PDDL
fragment or if they produce invalid plans.
- some planners crash due to not supporting the PDDL fragment
"""
class VerifyDataReport(PlanningReport):
    def get_text(self):
        assert len(self.algorithms) == 191
        algo_domains_with_invalid_plan = defaultdict(set)
        algo_domain_tasks_with_val_plan_too_long = defaultdict(lambda: defaultdict(list))
        algo_domain_tasks_with_unhandled_plan = defaultdict(lambda: defaultdict(list))
        domain_tasks_with_unhandled_plan = defaultdict(list)
        for run in self.props.values():
            """
            the if below currently triggers on domains with non-unit cost because UPV seemingly ignores
            missing "minimize total-cost metric" statements in problem files (which should probably not
            be missing in the first place...)
            """
            # if "val_cost" in run and run["upv_cost"] is not None and run["val_cost"] != run["upv_cost"]:
                # print("val and upv disagree")
                # print(run)
            if run["invalid_plan"]:
                algo_domains_with_invalid_plan[run["algorithm"]].add(run["domain"])
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
                #if not run["coverage"] and not run["invalid_plan"]:
                #    print("val cannot handle plan; upv apparently neither")
                #    print(run)
            if run["plan_files"] and not run["coverage"] and not run["invalid_plan"]:
                algo_domain_tasks_with_unhandled_plan[run["algorithm"]][run["domain"]].append((run["problem"], run["run_dir"]))
                domain_tasks_with_unhandled_plan[run["domain"]].append(run["problem"])
        lines = []
        lines.append("algos on problems with found plan files but no recorded coverage and no invalid plan")
        for algo, domain_tasks in algo_domain_tasks_with_unhandled_plan.items():
            lines.append(f"{algo}:")
            for domain, tasks in domain_tasks.items():
                lines.append(f"    {domain}:")
                line = [" ".join(task) for task in tasks]
                lines.append("        " + " ".join(line))
        lines.append("as list of domains with tasks:")
        for domain, tasks in domain_tasks_with_unhandled_plan.items():
            lines.append(f"{domain}: {sorted(tasks)},")
        """this is covered except the one task mentioned above"""
        # lines.append("algos on problems with too long val plan")
        # for algo, domain_tasks in algo_domain_tasks_with_val_plan_too_long.items():
            # lines.append(f"{algo}:")
            # for domain, tasks in domain_tasks.items():
                # lines.append(f"    {domain}:")
                # line = [" ".join(task) for task in tasks]
                # lines.append("        " + " ".join(line))
        """this was used to generate above dict invalid_plans"""
        # lines.append("algos on domains with invalid plans (val or upv)")
        # for algo, domains in algo_domains_with_invalid_plan.items():
            # lines.append(f"'{algo}': {sorted(domains)},")
        return "\n".join(lines)

exp.add_report(
    VerifyDataReport(
            attributes=HTML_ATTRIBUTES,
            filter=[filter_instances],
            format="txt",
        ),
        name=f"{exp.name}-verify")
exp.add_step("compress-properties", project.compress, Path(exp.eval_dir) / "properties")

class ProcessRuns:
    def __init__(self, track):
        self.track = track

    def __call__(self, run):
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
        FilterReport(filter=[filter_instances,exclude_planners_from_domains,quality_filter.store_costs,quality_filter.add_quality,process_runs]),
        outfile=properties_full,
        name=f"properties-full-{track}")
    exp.add_step(f"compress-properties-full-{track}", project.compress, properties_full)

for track in ["opt", "sat", "agl"]:
    process_runs = ProcessRuns(track)
    quality_filter = project.QualityFilters()
    properties_hardest = Path(exp.eval_dir) / f"properties-hardest-{track}.json"
    exp.add_report(
        project.Hardest30Report(filter=[filter_instances,exclude_planners_from_domains,quality_filter.store_costs,quality_filter.add_quality,process_runs]),
        outfile=properties_hardest,
        name=f"properties-hardest-{track}")
    exp.add_step(f"compress-properties-hardest-{track}", project.compress, properties_hardest)

exp.run_steps()
