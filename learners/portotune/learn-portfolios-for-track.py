#! /usr/bin/env python

from pathlib import Path

from lab.experiment import Experiment

import project
from portfolio import Track
from cluster import ClusterPortfolio
from domainwise import DomainwisePortfolio
from inc_timelimit import IncreasingTimelimitPortfolio
from ranitsearch import RanitSearchPortfolio
from selector import SelectorPortfolio, UniformPortfolio

DIR = Path(__file__).resolve().parent
REPO = project.get_repo_base()
BENCHMARKS_DIR = REPO / "benchmarks"
assert BENCHMARKS_DIR.exists(), BENCHMARKS_DIR
ATTRIBUTES = [
    "error",
    "coverage",
    "cost",
    "plan_length",
    "run_dir",
    "used_memory",
    "cpu_time",
]

SUFFIX = project.TRACKNAME
TRACK = Track[SUFFIX.upper()]
DATA = f"../../experiments/data/training-data-collect-eval/properties-hardest-{SUFFIX}.json.xz"
NUM_PLANNERS = {
    "agl": 100,
    "opt": 30,
    "sat": 80,
}[SUFFIX]

exp = Experiment(path=DIR / "data" / f"learn-{SUFFIX}-portfolios")
exp.add_step(
    "remove-eval-dir", project.remove_dir, Path(exp.eval_dir)
)

exp.add_fetcher(src=DATA, merge=True)

#project.add_absolute_report(exp, attributes=ATTRIBUTES, filter=[], name=f"{exp.name}-full")

exp.add_report(UniformPortfolio(track=TRACK), name=f"uniform-{SUFFIX}")

#exp.add_report(SelectorPortfolio(track=TRACK, subset_size="auto"), name=f"selector-{SUFFIX}")

for clusters in range(1, NUM_PLANNERS + 1):
    exp.add_report(ClusterPortfolio(track=TRACK, clusters=clusters), name=f"cluster-{clusters:02d}-{SUFFIX}")

for stepsize in [1, 5, 10, 30, 60, 120, 180, 240, 300, 600, 900, 1800]:
    exp.add_report(IncreasingTimelimitPortfolio(stepsize=stepsize, track=TRACK), name=f"increasing-time-limit-{stepsize}-{SUFFIX}")

exp.add_report(DomainwisePortfolio(track=TRACK), name=f"domain-wise-{SUFFIX}")

for random_seed in range(10):
    exp.add_report(RanitSearchPortfolio(track=TRACK, random_seed=random_seed), name=f"randomized-iterative-search-{random_seed}-{SUFFIX}")

exp.run_steps()
