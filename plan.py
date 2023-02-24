#! /usr/bin/env python

import argparse
from pathlib import Path
import subprocess
import traceback


DIR = Path(__file__).resolve().parent


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("image", help="path to Apptainer image file")
    parser.add_argument("config", help="one of config00, ..., config40 for FDSS image, ignored for other images")
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
    config = args.config
    if image_nick == "ipc2018-sat-fdss-2018":
        portfolio_path = DIR / "planners" / "ipc2018-sat-fdss-2018" / "driver" / "portfolios" / "seq_sat_fdss_2018.py"
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
    else:
        subprocess.run([image_path, args.domainfile, args.problemfile, args.planfile])


if __name__ == "__main__":
    main()
