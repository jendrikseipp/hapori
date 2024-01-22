#!/usr/bin/env python3
import argparse
from collections import defaultdict
import json
import lzma

# cpu_time, error, cost, used_memory, quality


parser = argparse.ArgumentParser()
# use this on properties for a *single* track; otherwise there would be duplicate planner+config pairs
parser.add_argument("file_properties")
parser.add_argument("property_key")
parser.add_argument("out_file")

def main(args):
    with lzma.open(args.file_properties, "r") as f:
        properties = json.load(f)

    data = defaultdict(lambda: defaultdict(dict))
    all_tasks = set()
    all_algorithms = set()
    for props in properties.values():
        algorithm = props["algorithm"][4:]
        all_algorithms.add(algorithm)
        domain = props["domain"]
        task = props["problem"]
        all_tasks.add((domain, task))
        value = props.get(args.property_key)
        if value is None:
            value = "-"
        data[domain][task][algorithm] = value
    all_algorithms = sorted(all_algorithms)
    all_tasks = sorted(all_tasks)

    # decstar config01 finds a suboptimal solution on these two problems
    # for algo in all_algorithms:
        # print(f"quality of {algo} on barman-strips.0-p02.pddl: {data['barman-strips']['0-p02.pddl'][algo]}")
        # print(f"quality of {algo} on barman-strips.0-p22.pddl: {data['barman-strips']['0-p22.pddl'][algo]}")

    with open(args.out_file, "w") as f:
        f.write(",".join([""] + all_algorithms))
        f.write("\n")
        for domain, task in all_tasks:
            f.write(f"{domain}:{task},")
            entries = [str(data[domain][task][algo]) for algo in all_algorithms]
            assert any(e != "-" for e in entries), f"{domain}, {task}"
            f.write(",".join(entries))
            f.write("\n")


if __name__ == "__main__":
    main(parser.parse_args())
