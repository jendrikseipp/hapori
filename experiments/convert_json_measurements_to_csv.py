#!/usr/bin/env python3
import argparse
from collections import defaultdict
import json
import lzma

#cpu_time, error, cost,plan_length, used_memory


parser = argparse.ArgumentParser()
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
        algorithm = props["algorithm"]
        all_algorithms.add(algorithm)
        domain = props["domain"]
        task = props["problem"]
        all_tasks.add((domain, task))
        time = props.get(args.property_key)
        if time is None:
            time = "-"
        data[domain][task][algorithm] = time
    all_algorithms = sorted(all_algorithms)
    all_tasks = sorted(all_tasks)

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