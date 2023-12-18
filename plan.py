#! /usr/bin/env python3

"""Run an Apptainer-based planner."""

import argparse
import itertools
from pathlib import Path
import subprocess
import sys
import traceback


DIR = Path(__file__).resolve().parent
CONTAINER_PLANNER_DIR = DIR / "planners"

"""
We use a single code base for all LAPKT BFWS planners even though the
competition versions used different builds:
- agile setting:
    - dual bfws uses a 64 bit build
    - polynomial bfws and bfws preference use a 32 bit build
- sat setting:
    - dual bfws and bfws preference use a 64 bit build
    - polynomial bfws use a 32 bit build
We decided to go with a single 64 bit build for all configurations.
"""
LAPKT_DRIVERS = {
    "dual-bfws-agl": "BFWS/fd-version/bfws.py",
    "dual-bfws-sat": "BFWS/fd-version/bfws_anytime_fd_singularity.py",
    "bfws-pref-agl": "BFWS/fd-version/bfws_f5.py",
    "bfws-pref-sat": "BFWS/fd-version/bfws_f5_anytime_fd_singularity.py",
    "poly-bfws": "BFWS/fd-version/poly_bfws.py",  # Same for agl and sat.
}

DELFI_CMDS = {
    'h2-simpless-dks-blind': ['--symmetries', 'sym=structural_symmetries(search_symmetries=dks)', '--search', 'astar(blind,symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    'h2-simpless-dks-celmcut': ['--symmetries', 'sym=structural_symmetries(search_symmetries=dks)', '--search', 'astar(celmcut,symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    'h2-simpless-dks-900masb50ksccdfp': ['--symmetries', 'sym=structural_symmetries(search_symmetries=dks)', '--search', 'astar(merge_and_shrink(shrink_strategy=shrink_bisimulation(greedy=false),merge_strategy=merge_sccs(order_of_sccs=topological,merge_selector=score_based_filtering(scoring_functions=[goal_relevance,dfp,total_order(atomic_before_product=false,atomic_ts_order=reverse_level,product_ts_order=new_to_old)])),label_reduction=exact(before_shrinking=true,before_merging=false),max_states=50000,threshold_before_merge=1,max_time=900),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    'h2-simpless-dks-900masb50ksbmiasm': ['--symmetries', 'sym=structural_symmetries(search_symmetries=dks)', '--search', 'astar(merge_and_shrink(shrink_strategy=shrink_bisimulation(greedy=false),merge_strategy=merge_stateless(merge_selector=score_based_filtering(scoring_functions=[sf_miasm(shrink_strategy=shrink_bisimulation,max_states=50000),total_order(atomic_before_product=true,atomic_ts_order=reverse_level,product_ts_order=old_to_new)])),label_reduction=exact(before_shrinking=true,before_merging=false),max_states=50000,threshold_before_merge=1,max_time=900),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    # we do not use h2 with miasm on purpose, because it suffers a lot from missing "left-over mutexes"
    'simpless-dks-masb50kmiasmdfp': ['--symmetries', 'sym=structural_symmetries(search_symmetries=dks)', '--search', 'astar(merge_and_shrink(shrink_strategy=shrink_bisimulation(greedy=false),merge_strategy=merge_precomputed(merge_tree=miasm(abstraction=miasm_merge_and_shrink(),fallback_merge_selector=score_based_filtering(scoring_functions=[goal_relevance,dfp,total_order(atomic_ts_order=reverse_level,product_ts_order=new_to_old,atomic_before_product=false)]))),label_reduction=exact(before_shrinking=true,before_merging=false),max_states=50000,threshold_before_merge=1,max_time=900),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    'h2-simpless-dks-900masginfsccdfp': ['--symmetries', 'sym=structural_symmetries(search_symmetries=dks)', '--search', 'astar(merge_and_shrink(shrink_strategy=shrink_bisimulation(greedy=true),merge_strategy=merge_sccs(order_of_sccs=topological,merge_selector=score_based_filtering(scoring_functions=[goal_relevance,dfp,total_order(atomic_before_product=false,atomic_ts_order=level,product_ts_order=random)])),label_reduction=exact(before_shrinking=true,before_merging=false),max_states=infinity,threshold_before_merge=1,max_time=900),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    'h2-simpless-dks-cpdbshc900': ['--symmetries', 'sym=structural_symmetries(search_symmetries=dks)', '--search', 'astar(cpdbs(patterns=hillclimbing(max_time=900),transform=multiply_out_conditional_effects),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    'h2-simpless-dks-zopdbsgenetic': ['--symmetries', 'sym=structural_symmetries(search_symmetries=dks)', '--search', 'astar(zopdbs(patterns=genetic(pdb_max_size=50000,num_collections=5,num_episodes=30,mutation_probability=0.01),transform=multiply_out_conditional_effects),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    'h2-simpless-oss-blind': ['--symmetries', 'sym=structural_symmetries(search_symmetries=oss)', '--search', 'astar(blind,symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    'h2-simpless-oss-celmcut': ['--symmetries', 'sym=structural_symmetries(search_symmetries=oss)', '--search', 'astar(celmcut,symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    'h2-simpless-oss-900masb50ksccdfp': ['--symmetries', 'sym=structural_symmetries(search_symmetries=oss)', '--search', 'astar(merge_and_shrink(shrink_strategy=shrink_bisimulation(greedy=false),merge_strategy=merge_sccs(order_of_sccs=topological,merge_selector=score_based_filtering(scoring_functions=[goal_relevance,dfp,total_order(atomic_before_product=false,atomic_ts_order=reverse_level,product_ts_order=new_to_old)])),label_reduction=exact(before_shrinking=true,before_merging=false),max_states=50000,threshold_before_merge=1,max_time=900,prune_unreachable_states=false),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    'h2-simpless-oss-900masb50ksbmiasm': ['--symmetries', 'sym=structural_symmetries(search_symmetries=oss)', '--search', 'astar(merge_and_shrink(shrink_strategy=shrink_bisimulation(greedy=false),merge_strategy=merge_stateless(merge_selector=score_based_filtering(scoring_functions=[sf_miasm(shrink_strategy=shrink_bisimulation,max_states=50000),total_order(atomic_before_product=true,atomic_ts_order=reverse_level,product_ts_order=old_to_new)])),label_reduction=exact(before_shrinking=true,before_merging=false),max_states=50000,threshold_before_merge=1,max_time=900,prune_unreachable_states=false),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    # we do not use h2 with miasm on purpose, because it suffers a lot from missing "left-over mutexes"
    'simpless-oss-masb50kmiasmdfp': ['--symmetries', 'sym=structural_symmetries(search_symmetries=oss)', '--search', 'astar(merge_and_shrink(shrink_strategy=shrink_bisimulation(greedy=false),merge_strategy=merge_precomputed(merge_tree=miasm(abstraction=miasm_merge_and_shrink(),fallback_merge_selector=score_based_filtering(scoring_functions=[goal_relevance,dfp,total_order(atomic_ts_order=reverse_level,product_ts_order=new_to_old,atomic_before_product=false)]))),label_reduction=exact(before_shrinking=true,before_merging=false),max_states=50000,threshold_before_merge=1,max_time=900,prune_unreachable_states=false),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    'h2-simpless-oss-masginfsccdfp': ['--symmetries', 'sym=structural_symmetries(search_symmetries=oss)', '--search', 'astar(merge_and_shrink(shrink_strategy=shrink_bisimulation(greedy=true),merge_strategy=merge_sccs(order_of_sccs=topological,merge_selector=score_based_filtering(scoring_functions=[goal_relevance,dfp,total_order(atomic_before_product=false,atomic_ts_order=level,product_ts_order=random)])),label_reduction=exact(before_shrinking=true,before_merging=false),max_states=infinity,threshold_before_merge=1,max_time=900,prune_unreachable_states=false),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    'h2-simpless-oss-cpdbshc900': ['--symmetries', 'sym=structural_symmetries(search_symmetries=oss)', '--search', 'astar(cpdbs(patterns=hillclimbing(max_time=900),transform=multiply_out_conditional_effects),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
    'h2-simpless-oss-zopdbsgenetic': ['--symmetries', 'sym=structural_symmetries(search_symmetries=oss)', '--search', 'astar(zopdbs(patterns=genetic(pdb_max_size=50000,num_collections=5,num_episodes=30,mutation_probability=0.01),transform=multiply_out_conditional_effects),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)'],
}

def get_portfolio_attributes(portfolio):
    attributes = {}
    with open(portfolio) as portfolio_file:
        content = portfolio_file.read()
        try:
            exec(content, attributes)
        except Exception:
            traceback.print_exc()
            raise ImportError(
                "The portfolio %s could not be loaded. Maybe it still "
                "uses the old portfolio syntax? See the FDSS portfolios "
                "for examples using the new syntax." % portfolio)
    if "CONFIGS" not in attributes:
        raise ValueError("portfolios must define CONFIGS")
    return attributes

FD_CONFIGS = get_portfolio_attributes(DIR / "configs" / "fd_2018_configs.py")["CONFIGS"]

CONFIGS = {
    "ipc2018-cerberus": ["sat", "agl", "sat-gl", "agl-gl"],
    "ipc2018-mercury2014": ["sat", "agl"],
    "ipc2018-merwin": ["sat", "agl"],
    "ipc2018-decstar": [f"opt-config{i:02d}" for i in range(0, 7)] + [f"agl-config{i:02d}" for i in range(0, 3)] + [f"sat-config{i:02d}" for i in range(0, 4)],
    "ipc2018-fd-2018": [f"config{i:02d}" for i in range(len(FD_CONFIGS))], # covers both fdss and fd-remix
    "ipc2018-lapkt-bfws": LAPKT_DRIVERS.keys(),
    "ipc2018-opt-delfi": DELFI_CMDS.keys(),
    "ipc2018-opt-metis": ["metis2"],  # Metis 1 is contained in the configurations of Delfi
    "ipc2018-saarplan": [f"sat-config{i:02d}" for i in range(2, 3)] + [f"agl-config{i:02d}" for i in range(1, 2)], # other non-used configs covered by decstar
    "ipc2018-symple1": ["symple100000OPT", "symple100000SAT", "symple100000AGL"],
    "ipc2018-symple2": ["symple100000OPT", "symple100000SAT", "symple100000AGL"],
}

SINGLE_CONFIG_PLANNERS = [
    "ipc2014-jasper", # sat + agl
    "ipc2014-agl-mpc",
    "ipc2014-agl-probe",
    "ipc2014-opt-symba1",
    "ipc2018-freelunch-madagascar", # sat + agl
    "ipc2018-olcff", # sat + agl
    "ipc2018-lapkt-dfs-plus", # sat + agl
    "ipc2018-opt-complementary2",
    "ipc2018-opt-planning-pdbs",
    "ipc2018-opt-scorpion",
]

for planner in SINGLE_CONFIG_PLANNERS:
    assert planner not in CONFIGS, planner
    CONFIGS[planner] = ["default"]


def get_configs_for_planner_and_track(planner, track):
    assert track in ["opt", "sat", "agl"]
    configs = CONFIGS[planner]
    result = []
    for config in configs:
        if any(track in x for x in [planner, config.lower()]):
            result.append(config)
        if planner == "ipc2018-lapkt-bfws" and config == "poly-bfws" and track in ["sat", "agl"]:
            result.append(config)
    if planner == "ipc2018-fd-2018" and track in ["sat", "agl"]:
        result = configs
    if planner == "ipc2014-jasper" and track in ["sat", "agl"]:
        result = configs
    if planner == "ipc2018-freelunch-madagascar" and track in ["sat", "agl"]:
        result = configs
    if planner == "ipc2018-olcff" and track in ["sat", "agl"]:
        result = configs
    if planner == "ipc2018-lapkt-dfs-plus" and track in ["sat", "agl"]:
        result = configs
    return result


def get_configs_for_track(track):
    assert track in ["opt", "sat", "agl"]
    result = {}
    for planner in CONFIGS.keys():
        result[planner] = get_configs_for_planner_and_track(planner, track)
    return result


def csv_list(s):
   return s.split(',')


def abs_path(arg):
    return str(Path(arg).resolve())


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("planner", type=Path, help="planner name (as the dir name under planners/)")
    parser.add_argument("--configs", help=f"Pass 'all' to run all configs. Possible values: {CONFIGS}", type=csv_list, default=["default"])
    parser.add_argument("domainfile", type=abs_path)
    parser.add_argument("problemfile", type=abs_path)
    parser.add_argument("planfile", type=abs_path)
    parser.add_argument("--check", action="store_true", help="Check planner exitcode and validate plans.")
    parser.add_argument("--list-configs", action="store_true", help="Show list of planner and config names and exit.")
    return parser.parse_args()


def prepare_config(config, replacements=None):
    replacements = [
        ("H_COST_TRANSFORM", "adapt_costs(one)"),
        ("H_COST_TYPE", "one"), # for decstar
        ("S_COST_TYPE", "one"),
        ("UNTIL_BOUND", "until_bound"), #  for saarplan to avoid below replacement
        ("BOUND", "infinity"),
    ] + (replacements or [])
    for index, part in enumerate(config):
        for before, after in replacements:
            part = part.replace(before, after)
        config[index] = part
    return config


def get_existing_plans(plan_prefix):
    plan_prefix = plan_prefix.resolve()
    if plan_prefix.exists():
        yield plan_prefix

    for counter in itertools.count(start=1):
        plan_filename = Path(f"{plan_prefix}.{counter}")
        if plan_filename.exists():
            yield plan_filename
        else:
            break


def run_planner(args, cmd):
    print(f"Calling planner: {cmd}")
    subprocess.run(cmd, check=args.check)
    plan_prefix = Path(args.planfile).resolve()
    existing_plan_files = [str(plan) for plan in get_existing_plans(plan_prefix)]

    print(f"Found plan file(s): {existing_plan_files}")
    if existing_plan_files:
        if args.check:
            subprocess.check_call(["validate", "-L", "-v", args.domainfile, args.problemfile] + existing_plan_files)
    else:
        # We use --config=all only for testing small instances, so exit here if one config fails to solve the easy task.
        sys.exit(99)

def main():
    args = parse_args()
    if args.list_configs:
        for planner, configs in sorted(CONFIGS.items()):
            for config in sorted(configs):
                print(f"(1, ['{planner}', '{config}']),")
        sys.exit()

    planner = str(args.planner)
    if planner not in CONFIGS:
        sys.exit(f"unknown planner {planner}")

    configs = args.configs
    print(f"Planner: {planner}")
    print(f"Configs: {configs}")
    print(f"Plan file: {args.planfile}")
    if not configs:
        sys.exit(f"Planner {planner} needs at least one config from {list(CONFIGS[planner])}.")
    for config in configs:
        if config == "all":
            configs = CONFIGS[planner]
        elif config not in CONFIGS[planner]:
            sys.exit(f"Planner {planner} does not support config {config}.")

    for config in configs:
        print(f"Run planner config {config}")
        if planner == "ipc2018-fd-2018":
            assert config.startswith("config"), config
            config_index = int(config[len("config"):])
            assert 0 <= config_index < len(FD_CONFIGS)
            fd_config = prepare_config(FD_CONFIGS[config_index])
            cmd = [
                "python2",
                f"{CONTAINER_PLANNER_DIR}/{planner}/fast-downward.py", "--build=release64",
                "--plan-file", args.planfile,
                "--transform-task", f"{CONTAINER_PLANNER_DIR}/{planner}/builds/h2-mutexes/bin/preprocess",
                args.domainfile, args.problemfile] + fd_config
            run_planner(args, cmd)
        elif planner == "ipc2018-lapkt-bfws":
            cmd = ["python2", f"{CONTAINER_PLANNER_DIR}/{planner}/{LAPKT_DRIVERS[config]}", args.domainfile, args.problemfile, args.planfile]
            run_planner(args, cmd)
        elif planner == "ipc2018-decstar":
            track, temp = config.split('-')
            assert track in ['agl', 'sat', 'opt'], track
            portfolio_path = DIR / "configs" / f"seq_{track}_ds.py"
            ds_configs = get_portfolio_attributes(portfolio_path)["CONFIGS"]
            if track == "sat":
                ds_configs.append(
                # FINAL_CONFIG of portfolio
                (-1,[
                    "--heuristic",
                    "hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true,"
                    "                         lm_cost_type=H_COST_TYPE,cost_type=H_COST_TYPE))",
                    "--search", """iterated([
                                     lazy_greedy([hff,hlm],preferred=[hff,hlm],
                                                 cost_type=one,reopen_closed=false),
                                     lazy_greedy([hff,hlm],preferred=[hff,hlm],
                                                 reopen_closed=false),
                                     lazy_wastar([hff,hlm],preferred=[hff,hlm],w=5),
                                     lazy_wastar([hff,hlm],preferred=[hff,hlm],w=3),
                                     lazy_wastar([hff,hlm],preferred=[hff,hlm],w=2),
                                     lazy_wastar([hff,hlm],preferred=[hff,hlm],w=1)
                                     ],repeat_last=true,continue_on_fail=true, bound=BOUND)"""
                ]))
            assert temp.startswith("config"), temp
            config_index = int(temp[len("config"):])
            assert 0 <= config_index < len(ds_configs)
            _, ds_config = ds_configs[config_index]
            ds_config = prepare_config(ds_config)
            h2_time_limit = {
                'agl' : 10,
                'sat' : 30,
                'opt' : 120,
            }
            cmd = [
                "python2", f"{CONTAINER_PLANNER_DIR}/{planner}/src/fast-downward.py",
                "--plan-file", args.planfile,
                args.domainfile, args.problemfile,
                "--preprocess-options", "--h2-time-limit", f"{h2_time_limit[track]}",
                "--search-options"] + ds_config
            run_planner(args, cmd)
        elif planner == "ipc2018-saarplan":
            track, temp = config.split('-')
            assert track in ['agl', 'sat'], track
            portfolio_path = DIR / "configs" / f"seq_{track}_saarplan.py"
            saarplan_configs = get_portfolio_attributes(portfolio_path)["CONFIGS"]
            assert temp.startswith("config"), temp
            config_index = int(temp[len("config"):])
            assert 0 <= config_index < len(saarplan_configs)
            _, saarplan_config = saarplan_configs[config_index]
            saarplan_config = prepare_config(saarplan_config)
            h2_time_limit = {
                'agl' : 10,
                'sat' : 30,
            }
            cmd = [
                "python2",
                f"{CONTAINER_PLANNER_DIR}/{planner}/fast-downward.py",
                "--plan-file", args.planfile,
                args.domainfile, args.problemfile,
                "--preprocess-options", "--h2_time_limit", f"{h2_time_limit[track]}",
                "--search-options"]
            cmd.extend(saarplan_config)
            run_planner(args, cmd)
        elif planner == "ipc2018-symple1" or planner == "ipc2018-symple2":
            if "OPT" in config:
                h2_time_limit = "60"
            elif "SAT" in config:
                h2_time_limit = "300"
            elif "AGL" in config:
                h2_time_limit = "60"
            else:
                sys.exit("unknown config for Symple")
            cmd = [f"{CONTAINER_PLANNER_DIR}/{planner}/src/plan", args.domainfile, args.problemfile, h2_time_limit, "ipc-18", config, "--plan-file", args.planfile]
            run_planner(args, cmd)
        elif planner == "ipc2018-opt-delfi":
            cmd = ["python2", f"{CONTAINER_PLANNER_DIR}/{planner}/fast-downward.py", "--build", "release64"]
            if "masb50kmiasmdfp" not in config:
                cmd.extend(["--transform-task", f"{CONTAINER_PLANNER_DIR}/{planner}/builds/release64/bin/preprocess"])
            cmd.extend(["--plan-file", args.planfile, args.domainfile, args.problemfile] + DELFI_CMDS[config])
            run_planner(args, cmd)
        elif planner == "ipc2018-opt-metis":
            assert config == "metis2"
            cmd = ["python2", f"{CONTAINER_PLANNER_DIR}/{planner}/fast-downward.py", "--build=release64", "--plan-file", args.planfile, "--transform-task", f"{CONTAINER_PLANNER_DIR}/{planner}/builds/release64/bin/preprocess", "--overall-time-limit", "30m", args.domainfile, args.problemfile, "--symmetries", "sym=structural_symmetries(search_symmetries=dks)", "--search", "astar(max([celmcut,lmcount(lm_factory=lm_merged([lm_rhw,lm_hm(m=1)]),admissible=true,transform=multiply_out_conditional_effects)]),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)"]
            run_planner(args, cmd)
        elif planner == "ipc2018-mercury2014":
            cmd = [f"{CONTAINER_PLANNER_DIR}/{planner}/src/plan-ipc", f"seq-{config}-mercury", args.domainfile, args.problemfile, args.planfile]
            run_planner(args, cmd)
        elif planner == "ipc2018-cerberus":
            cmd = ["python2", f"{CONTAINER_PLANNER_DIR}/{planner}/plan.py", args.domainfile, args.problemfile, args.planfile, config]
            run_planner(args, cmd)
        elif planner == "ipc2018-opt-scorpion":
            cmd = ["python2", f"{CONTAINER_PLANNER_DIR}/{planner}/fast-downward.py", "--build=release64", "--plan-file", args.planfile, "--transform-task", f"{CONTAINER_PLANNER_DIR}/{planner}/builds/h2-mutexes/bin/preprocess", "--alias", "seq-opt-scorpion", "--overall-time-limit", "30m", args.domainfile, args.problemfile]
            run_planner(args, cmd)
        elif planner == "ipc2018-opt-complementary2":
            cmd = ["python2", f"{CONTAINER_PLANNER_DIR}/{planner}/fast-downward.py", "--build=release64", "--plan-file", args.planfile, args.domainfile, args.problemfile, "--search", "astar(cpdbs_symbolic(genetic_ss(use_ucb=true,num_episodes=10000000,num_collections=1,pdb_factory=symbolic,genetic_time_limit=900,time_limit=1.0,create_perimeter=true,use_first_goal_vars=true,use_norm_dist=true)))"]
            run_planner(args, cmd)
        elif planner == "ipc2014-opt-symba1":
            cmd = [f"{CONTAINER_PLANNER_DIR}/{planner}/plan", "seq-opt-symba-1", args.domainfile, args.problemfile, args.planfile]
            run_planner(args, cmd)
        elif planner == "ipc2018-merwin":
            cmd = [f"{CONTAINER_PLANNER_DIR}/{planner}/plan-{config}", args.domainfile, args.problemfile, args.planfile]
            run_planner(args, cmd)
        elif planner == "ipc2018-lapkt-dfs-plus":
            cmd = ["python2", f"{CONTAINER_PLANNER_DIR}/{planner}/LAPKT-public/planners/dfs_plus/dfs_plus.py", args.domainfile, args.problemfile, args.planfile]
            run_planner(args, cmd)
        elif planner == "ipc2018-freelunch-madagascar":
            cmd = [f"{CONTAINER_PLANNER_DIR}/{planner}/plan.sh", args.domainfile, args.problemfile, args.planfile, f"{CONTAINER_PLANNER_DIR}/{planner}/MpC", f"{CONTAINER_PLANNER_DIR}/{planner}/incplan-lgl"]
            run_planner(args, cmd)
        elif planner == "ipc2018-opt-planning-pdbs":
            cmd = ["python", f"{CONTAINER_PLANNER_DIR}/{planner}/fast-downward.py",
            "--build=release64",
            "--plan-file", args.planfile,
            args.domainfile, args.problemfile,
            "--search", "astar(cpdbs_symbolic(genetic_ss(use_ucb=true,num_episodes=10000000,num_collections=1,pdb_factory=symbolic,genetic_time_limit=900,time_limit=1.0,create_perimeter=true,use_first_goal_vars=false,use_norm_dist=true)))"]
            run_planner(args, cmd)
        elif planner == "ipc2014-agl-mpc":
            cmd = [f"{CONTAINER_PLANNER_DIR}/{planner}/MpC", args.domainfile, args.problemfile, "-o", args.planfile, "-Q"]
            run_planner(args, cmd)
        elif planner == "ipc2014-jasper":
            cmd = [f"{CONTAINER_PLANNER_DIR}/{planner}/plan", args.domainfile, args.problemfile, args.planfile]
            run_planner(args, cmd)
        elif planner == "ipc2014-agl-probe":
            cmd = [f"{CONTAINER_PLANNER_DIR}/{planner}/plan", args.domainfile, args.problemfile, args.planfile]
            run_planner(args, cmd)
        elif planner == "ipc2018-olcff":
            cmd = ["python2", f"{CONTAINER_PLANNER_DIR}/{planner}/fast-downward-conjunctions/fast-downward.py",
            "--build=release64",
            "--plan-file", args.planfile,
            args.domainfile, args.problemfile,
            "--search-options",
            "--heuristic", "hcff=cff(seed=42, cache_estimates=false, cost_type=ONE)",
            "--heuristic", "hn=novelty(cache_estimates=false)",
            "--heuristic", "tmp=novelty_linker(hcff, [hn])",
            "--heuristic", "hlm=lmcount(lm_rhw(reasonable_orders=true, lm_cost_type=ONE), cost_type=ONE)",
            "--search", "ipc18_iterated([ehc_cn(hcff, preferred=hcff, novelty=hn, seed=42, cost_type=ONE, max_growth=8, max_time=180), lazy_greedy_c([hcff, hlm], preferred=[hcff], conjunctions_heuristic=hcff, strategy=maintain_fixed_size_probabilistic(initial_removal_mode=UNTIL_BOUND, base_probability=0.02, target_growth_ratio=1.50), cost_type=ONE)], continue_on_solve=false, continue_on_fail=true, delete_after_phase_heuristics=[hn, tmp], delete_after_phase_phases=[0, 0])",
            "--translate-options", "--invariant-generation-max-time", "30",
            "--preprocess-options", "--h2_time_limit", "30"]
            run_planner(args, cmd)
        else:
            sys.exit(f"planner {planner} not handled!")


if __name__ == "__main__":
    main()
