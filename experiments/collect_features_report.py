from collections import defaultdict
import json
import logging
import numpy as np
from pathlib import Path
import re
import shutil
import sys

from downward.reports import PlanningReport


def natural_sort_tuple(s):
    def convert(_s):
        return int(_s) if _s.isdigit() else _s.lower()
    return [convert(c) for c in re.split('([0-9]+)', s)]


class CollectFeaturesReport(PlanningReport):
    def __init__(self, source_file, directory_name, is_json, **kwargs):
        PlanningReport.__init__(self, **kwargs)
        self.source_file = source_file
        self.directory_name = directory_name
        self.is_json = is_json

    def write(self):
        if len(self.algorithms) != 1:
            logging.critical("report needs exactly one algorithm")
        exp_dir = Path(self.eval_dir.replace("-eval", ""))
        assert exp_dir.is_dir()
        out_dir = Path(self.eval_dir) / self.directory_name
        assert not out_dir.exists()

        domain2runs = defaultdict(list)
        for (domain, problem), runs in self.problem_runs.items():
            assert len(runs) == 1
            domain2runs[domain].append(runs[0])
        if self.is_json:
            data = {}
            for domain, runs in domain2runs.items():
                data_domain = {}
                for run in runs:
                    source_file = exp_dir / run["run_dir"] / self.source_file
                    if not source_file.exists():
                        print(f"Missing {source_file}")
                        continue
                    with open(source_file, "r") as f:
                        data_domain[run["problem"]] = json.load(f)
                data[domain] = data_domain

            with open(Path(self.eval_dir) / self.directory_name, "w") as f:
                json.dump(data, f, sort_keys=True, indent=4)
            pass
        else:
            for domain, runs in domain2runs.items():
                domain_dir = out_dir / domain
                domain_dir.mkdir(parents=True, exist_ok=True)
                for run in runs:
                    source_file = exp_dir / run["run_dir"] / self.source_file
                    target_file = str(domain_dir / run["problem"]) +  source_file.suffix
                    if source_file.exists():
                        shutil.copy(source_file, target_file)
