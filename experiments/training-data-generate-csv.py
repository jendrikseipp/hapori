#!/usr/bin/env python3

from collections import defaultdict
import json
import lzma

ATTRIBUTES = [
    "cost",
    "cpu_time",
    "error",
    "quality",
    "used_memory",
]

def main():
    for properties_file in [
        "properties-full-agl.json.xz",
        "properties-full-opt.json.xz",
        "properties-full-sat.json.xz",
        "properties-hardest-agl.json.xz",
        "properties-hardest-opt.json.xz",
        "properties-hardest-sat.json.xz",
    ]:
        with lzma.open(f"data/training-data-collect-eval/{properties_file}", "r") as f:
            properties = json.load(f)

        data = defaultdict(lambda: defaultdict(dict))
        all_tasks = set()
        all_algorithms = set()
        track = None
        for props in properties.values():
            t = props["algorithm"][:3]
            if track is None:
                track = t
            assert track == t # each file contains data for a single track
            algorithm = props["algorithm"][4:]
            all_algorithms.add(algorithm)
            domain = props["domain"]
            task = props["problem"]
            all_tasks.add((domain, task))
            for attribute in ATTRIBUTES:
                value = props.get(attribute)
                if value is None:
                    value = "-"
                data[(domain,task)][algorithm][attribute] = value
        all_algorithms = sorted(all_algorithms)
        all_tasks = sorted(all_tasks)

        for attribute in ATTRIBUTES:
            outfile = properties_file.replace("properties-", "").replace(".json.xz", "") + f"-{attribute}.csv"
            with open(f"data/training-data-collect-eval/{outfile}", "w") as f:
                f.write(",".join([""] + all_algorithms))
                f.write("\n")
                for domain, task in all_tasks:
                    f.write(f"{domain}:{task},")
                    entries = [str(data[(domain,task)][algo][attribute]) for algo in all_algorithms]
                    # assert any(e != "-" for e in entries), f"{domain}, {task}"
                    f.write(",".join(entries))
                    f.write("\n")

if __name__ == "__main__":
    main()
