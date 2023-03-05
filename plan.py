#! /usr/bin/env python

"""Run an Apptainer-based planner."""

import argparse
import os
from pathlib import Path
import subprocess
import sys
import traceback


DIR = Path(__file__).resolve().parent

LAPKT_DRIVERS = {
    "dual-bfws": "/planner/BFWS/fd-version/bfws.py",
    "bfws-pref": "/planner/BFWS/fd-version/bfws_f5.py",
    "poly-bfws": "/planner/BFWS/fd-version/poly_bfws.py",
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

CONFIGS = {
    "ipc2018-opt-decstar": [f"config{i:02d}" for i in range(0, 7)],
    "ipc2018-opt-delfi": DELFI_CMDS.keys(),
    # "ipc2018-opt-fdms": ["fdms1", "fdms2"], # covered by Delfi
    # "ipc2018-opt-metis": ["metis1", "metis2"],  # Metis 1 is contained in the configurations of Delfi
    "ipc2018-opt-metis": ["metis2"],
    "ipc2018-agl-cerberus": ["sat", "agl", "sat-gl", "agl-gl"],
    "ipc2018-agl-fdss-2018": [f"config{i:02d}" for i in range(0, 41)],
    #"ipc2018-agl-ibacop": ["arvand", "probe", "yahsp2-mt"],
    "ipc2018-agl-lapkt-bfws": LAPKT_DRIVERS.keys(),
    "ipc2018-agl-mercury2014": ["sat", "agl"],
    "ipc2018-agl-merwin": ["sat", "agl"],
}

def csv_list(s):
   return s.split(',')


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("image", help="path to or nick for Apptainer image file")
    parser.add_argument("--configs", help=f"required for images {', '.join(CONFIGS.keys())} and forbidden for other images. Pass 'all' to run all configs. Possible values: {CONFIGS}", type=csv_list, default=[])
    parser.add_argument("domainfile")
    parser.add_argument("problemfile")
    parser.add_argument("planfile")
    parser.add_argument("--not-check-subprocess", action="store_false")
    return parser.parse_args()


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


def prepare_config(config, replacements=None):
    replacements = [
        ("H_COST_TRANSFORM", "adapt_costs(one)"),
        ("S_COST_TYPE", "one"),
        ("BOUND", "infinity"),
    ] + (replacements or [])
    for index, part in enumerate(config):
        for before, after in replacements:
            part = part.replace(before, after)
        config[index] = part
    return config

def run_image(args, cmd):
    subprocess.run(cmd, check=args.not_check_subprocess)
    if os.path.exists(args.planfile):
        print("Found plan file.")
        subprocess.call(["validate", "-L", "-v", args.domainfile, args.problemfile, args.planfile])
    else:
        print("No plan file.")
        # TODO why do we support running multiple configuations in a single
        #  call? If this line should be reworked if there is really is a use
        # case for running multiple configs at once.
        sys.exit(99)

def main():
    args = parse_args()
    if Path(args.image).exists():
        image_path = args.image
        image_nick = Path(Path(args.image).name).stem
    else:
        image_nick = args.image
        image_path = DIR / "images" / f"{image_nick}.img"
    configs = args.configs
    print(f"Image path: {image_path}")
    print(f"Image nick: {image_nick}")
    print(f"Configs: {configs}")
    if image_nick in CONFIGS:
        if not configs:
            sys.exit(f"Image {image_nick} needs at least one config.")
        for config in configs:
            if config == "all":
                configs = CONFIGS[image_nick]
            elif config not in CONFIGS[image_nick]:
                sys.exit(f"Image {image_nick} does not support config {config}.")
    elif configs and configs != ["all"]:
        sys.exit(f"The --configs parameter is only allowed for the images {list(CONFIGS.keys())}")
    else:
        run_image(args, [image_path, args.domainfile, args.problemfile, args.planfile])

    for config in configs:
        print(f"Run config {config}")
        if image_nick == "ipc2018-agl-fdss-2018":
            portfolio_path = DIR / "planners" / "ipc2018-agl-fdss-2018" / "driver" / "portfolios" / "seq_sat_fdss_2018.py"
            fdss_configs = get_portfolio_attributes(portfolio_path)["CONFIGS"]
            print(f"FDSS configs: {len(fdss_configs)}")
            assert config.startswith("config"), config
            config_index = int(config[len("config"):])
            assert 0 <= config_index < len(fdss_configs)
            _, fdss_config = fdss_configs[config_index]
            fdss_config = prepare_config(fdss_config)
            run_image(args, [
                image_path, "--build=release64",
                "--plan-file", args.planfile,
                "--transform-task", "/planner/preprocess",
                args.domainfile, args.problemfile] + fdss_config)
        elif image_nick == "ipc2018-agl-lapkt-bfws":
            run_image(args, [
                image_path, LAPKT_DRIVERS[config], args.domainfile, args.problemfile, args.planfile])
        elif image_nick == "ipc2018-opt-decstar":
            portfolio_path = DIR / "planners" / "ipc2018-opt-decstar" / "src" / "driver" / "portfolios" / "seq_opt_ds.py"
            ds_configs = get_portfolio_attributes(portfolio_path)["CONFIGS"]
            print(f"Decstar configs: {len(ds_configs)}")
            assert config.startswith("config"), config
            config_index = int(config[len("config"):])
            assert 0 <= config_index < len(ds_configs)
            _, ds_config = ds_configs[config_index]
            ds_config = prepare_config(ds_config)
            run_image(args, [
                image_path,
                "--plan-file", args.planfile,
                args.domainfile, args.problemfile,
                "--preprocess-options", "--h2-time-limit", "120",
                "--search-options"] + ds_config)
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
        elif image_nick in ["ipc2018-agl-cerberus", "ipc2018-agl-merwin", "ipc2018-agl-mercury2014"]:
            run_image(args, [
                image_path, args.domainfile, args.problemfile, args.planfile, config])






if __name__ == "__main__":
    main()
