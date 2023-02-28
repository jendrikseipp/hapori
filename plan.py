#! /usr/bin/env python

"""Run an Apptainer-based planner."""

import argparse
from pathlib import Path
import subprocess
import sys
import traceback


DIR = Path(__file__).resolve().parent
CONFIGS = {
    "ipc2018-opt-fdms": ["fdms1", "fdms2"],
    "ipc2018-agl-fdss-2018": [f"config{i:02d}" for i in range(1, 41)],
}

def csv_list(s):
   return s.split(',')


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("image", help="path to Apptainer image file")
    parser.add_argument("--configs", help=f"required for images {', '.join(CONFIGS.keys())} and forbidden for other images. Possible values: {CONFIGS}", type=csv_list, default=[])
    parser.add_argument("domainfile")
    parser.add_argument("problemfile")
    parser.add_argument("planfile")
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


def main():
    args = parse_args()
    image_path = args.image
    image_nick = Path(Path(args.image).name).stem
    configs = args.configs
    print(f"Image nick: {image_nick}")
    print(f"Configs: {configs}")
    if image_nick in CONFIGS:
        if not configs:
            sys.exit(f"Image {image_nick} needs at least one config.")
        for config in configs:
            if config not in CONFIGS[image_nick]:
                sys.exit(f"Image {image_nick} does not support config {config}.")
    elif configs:
        sys.exit(f"The --configs parameter is only allowed for the images {list(CONFIGS.keys())}")
    else:
        subprocess.run([image_path, args.domainfile, args.problemfile, args.planfile])

    for config in configs:
        if image_nick == "ipc2018-agl-fdss-2018":
            portfolio_path = DIR / "planners" / "ipc2018-agl-fdss-2018" / "driver" / "portfolios" / "seq_sat_fdss_2018.py"
            configs = get_portfolio_attributes(portfolio_path)["CONFIGS"]
            print(f"Configs: {len(configs)}")
            assert config.startswith("config"), config
            config_index = int(config[len("config"):])
            assert 0 <= config_index < len(configs)
            _, config = configs[config_index]
            config = prepare_config(config)
            subprocess.run([
                image_path, "--build=release64",
                "--plan-file", args.planfile,
                "--transform-task", "/planner/preprocess",
                args.domainfile, args.problemfile] + config)


if __name__ == "__main__":
    main()
