#! /usr/bin/env python

"""Run an Apptainer-based planner."""

import argparse
import itertools
from pathlib import Path
import subprocess
import sys
import tempfile
import traceback


DIR = Path(__file__).resolve().parent

LAPKT_DRIVERS = {
    "dual-bfws-agl": "/planner/BFWS/fd-version/bfws.py",
    "dual-bfws-sat": "/planner/BFWS/fd-version/bfws_anytime_fd_singularity.py",
    "bfws-pref-agl": "/planner/BFWS/fd-version/bfws_f5.py",
    "bfws-pref-sat": "/planner/BFWS/fd-version/bfws_f5_anytime_fd_singularity.py",
    "poly-bfws": "/planner/BFWS/fd-version/poly_bfws.py",  # Same for agl and sat.
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
    "ipc2018-agl-cerberus": ["sat", "agl", "sat-gl", "agl-gl"],
    #"ipc2018-agl-freelunch-doubly-relaxed": ["sat", "agl"],  # Image is too large.
    "ipc2018-agl-mercury2014": ["sat", "agl"],
    "ipc2018-agl-merwin": ["sat", "agl"],
    "ipc2018-decstar": [f"opt-config{i:02d}" for i in range(0, 7)] + [f"agl-config{i:02d}" for i in range(0, 3)] + [f"sat-config{i:02d}" for i in range(0, 4)],
    "ipc2018-fd-2018": [f"config{i:02d}" for i in range(len(FD_CONFIGS))],
    "ipc2018-lapkt-bfws": LAPKT_DRIVERS.keys(),
    "ipc2018-opt-delfi": DELFI_CMDS.keys(),
    # "ipc2018-opt-fdms": ["fdms1", "fdms2"], # covered by Delfi
    "ipc2018-opt-metis": ["metis2"],  # Metis 1 is contained in the configurations of Delfi
    "ipc2018-saarplan": [f"sat-config{i:02d}" for i in range(2, 3)] + [f"agl-config{i:02d}" for i in range(1, 2)],
    "ipc2018-symple1": ["symple100000OPT", "symple100000SAT", "symple100000AGL"],
    "ipc2018-symple2": ["symple100000OPT", "symple100000SAT", "symple100000AGL"],
}

SINGLE_CONFIG_IMAGES = [
    "ipc2014-agl-jasper",
    "ipc2014-agl-mpc",
    "ipc2014-agl-probe",
    "ipc2014-opt-symba1",
    "ipc2018-agl-freelunch-madagascar",
    "ipc2018-agl-olcff",
    "ipc2018-lapkt-dfs-plus",
    #"ipc2018-opt-complementary1",  # Suboptimal.
    "ipc2018-opt-complementary2",
    "ipc2018-opt-planning-pdbs",
    "ipc2018-opt-scorpion",
]

for image_nick in SINGLE_CONFIG_IMAGES:
    assert image_nick not in CONFIGS, image_nick
    CONFIGS[image_nick] = ["default"]


def check_consistency():
    defined_images = set(CONFIGS)
    existing_images = set(Path(path.name).stem for path in (DIR / "images").glob("*.img"))
    missing_images = defined_images - existing_images
    undefined_images = existing_images - defined_images
    print("Defined images:", sorted(defined_images))
    print("Existing images:", sorted(existing_images))
    print("Missing images:", sorted(missing_images))
    print("Undefined images:", sorted(undefined_images))


def csv_list(s):
   return s.split(',')


def abs_path(arg):
    return str(Path(arg).resolve())


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("image", type=Path, help="path to or nick for Apptainer image file")
    parser.add_argument("--configs", help=f"Pass 'all' to run all configs. Possible values: {CONFIGS}", type=csv_list, default=["default"])
    parser.add_argument("domainfile", type=abs_path)
    parser.add_argument("problemfile", type=abs_path)
    parser.add_argument("planfile", type=abs_path)
    parser.add_argument("--check", action="store_true", help="Check planner exitcode and validate plans.")
    parser.add_argument("--list-configs", action="store_true", help="Show list of image and config names and exit.")
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


def run_image(args, cmd):
    with tempfile.TemporaryDirectory() as d:
        subprocess.run(cmd, check=args.check, cwd=d)
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
    #check_consistency()
    if args.list_configs:
        for image_nick, configs in sorted(CONFIGS.items()):
            for config in sorted(configs):
                print(f"(1, ['{image_nick}', '{config}']),")
        sys.exit()

    if args.image.exists():
        image_path = args.image
        image_nick = Path(image_path.name).stem
    else:
        image_nick = str(args.image)
        image_path = DIR / "images" / f"{image_nick}.img"
    image_path = image_path.resolve()

    configs = args.configs
    print(f"Image path: {image_path}")
    print(f"Image nick: {image_nick}")
    print(f"Configs: {configs}")
    print(f"Plan file: {args.planfile}")
    if image_nick in CONFIGS:
        if not configs:
            sys.exit(f"Image {image_nick} needs at least one config from {list(CONFIGS[image_nick])}.")
        for config in configs:
            if config == "all":
                configs = CONFIGS[image_nick]
            elif config not in CONFIGS[image_nick]:
                sys.exit(f"Image {image_nick} does not support config {config}.")
    elif len(configs) == 1 and configs[0] in {"all", "default"}:
        run_image(args, [image_path, args.domainfile, args.problemfile, args.planfile])
        return

    for config in configs:
        print(f"Run image config {config}")
        if image_nick == "ipc2018-fd-2018":
            assert config.startswith("config"), config
            config_index = int(config[len("config"):])
            assert 0 <= config_index < len(FD_CONFIGS)
            fd_config = prepare_config(FD_CONFIGS[config_index])
            run_image(args, [
                image_path, "--build=release64",
                "--plan-file", args.planfile,
                "--transform-task", "/planner/preprocess",
                args.domainfile, args.problemfile] + fd_config)
        elif image_nick == "ipc2018-lapkt-bfws":
            run_image(args, [
                image_path, LAPKT_DRIVERS[config], args.domainfile, args.problemfile, args.planfile])
        elif image_nick == "ipc2018-decstar":
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
            run_image(args, [
                image_path,
                "--plan-file", args.planfile,
                args.domainfile, args.problemfile,
                "--preprocess-options", "--h2-time-limit", f"{h2_time_limit[track]}",
                "--search-options"] + ds_config)
        elif image_nick == "ipc2018-saarplan":
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
            run_image(args, [
                image_path,
                "--plan-file", args.planfile,
                args.domainfile, args.problemfile,
                "--preprocess-options", "--h2_time_limit", f"{h2_time_limit[track]}",
                "--search-options"] + saarplan_config)
        elif image_nick == "ipc2018-symple1" or image_nick == "ipc2018-symple2":
            if "OPT" in config:
                h2_time_limit = "60"
            elif "SAT" in config:
                h2_time_limit = "300"
            elif "AGL" in config:
                h2_time_limit = "60"
            else:
                sys.exit("unknown config for Symple")
            run_image(args, [
                image_path, args.domainfile, args.problemfile, args.planfile, config, h2_time_limit])
        elif image_nick == "ipc2018-opt-delfi":
            preprocess = ""
            if "masb50kmiasmdfp" not in config:
                preprocess = "--transform-task preprocess"
            run_image(
                args, [
                image_path, args.domainfile, args.problemfile, args.planfile, preprocess, " ".join(DELFI_CMDS[config])])
        elif image_nick == "ipc2018-opt-fdms":
            if config == "fdms1":
                merge_strategy = "merge_strategy=merge_sccs(order_of_sccs=topological,merge_selector=score_based_filtering(scoring_functions=[goal_relevance,dfp,total_order(atomic_ts_order=reverse_level,product_ts_order=new_to_old,atomic_before_product=false)]))"
            elif config == "fdms2":
                merge_strategy = "merge_strategy=merge_stateless(merge_selector=score_based_filtering(scoring_functions=[sf_miasm(shrink_strategy=shrink_bisimulation(greedy=false),max_states=50000,threshold_before_merge=1),total_order(atomic_ts_order=reverse_level,product_ts_order=new_to_old,atomic_before_product=false)]))"
            else:
                sys.exit(f"unknown config {config}")
            run_image(args, [
                image_path, args.domainfile, args.problemfile, args.planfile, merge_strategy])
        elif image_nick == "ipc2018-opt-metis":
            if config == "metis1":
                cmd = "--symmetries sym=structural_symmetries(search_symmetries=oss) --search astar(celmcut,symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)"
            elif config == "metis2":
                cmd = "--symmetries sym=structural_symmetries(search_symmetries=dks) --search astar(max([celmcut,lmcount(lm_factory=lm_merged([lm_rhw,lm_hm(m=1)]),admissible=true,transform=multiply_out_conditional_effects)]),symmetries=sym,pruning=stubborn_sets_simple(minimum_pruning_ratio=0.01),num_por_probes=1000)"
            else:
                sys.exit(f"unknown config {config}")
            run_image(args, [
                image_path, args.domainfile, args.problemfile, args.planfile, cmd])
        elif image_nick == "ipc2018-agl-ibacop":
            run_image(args, [
                image_path, args.domainfile, args.problemfile, args.planfile, config])
        elif image_nick in ["ipc2018-agl-cerberus", "ipc2018-agl-merwin", "ipc2018-agl-mercury2014", "ipc2018-agl-freelunch-doubly-relaxed"]:
            run_image(args, [
                image_path, args.domainfile, args.problemfile, args.planfile, config])
        else:
            run_image(args, [
                image_path, args.domainfile, args.problemfile, args.planfile])


if __name__ == "__main__":
    main()
