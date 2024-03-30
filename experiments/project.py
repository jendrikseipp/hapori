from collections import defaultdict
from pathlib import Path
import platform
import re
import subprocess
import sys
import traceback

from downward.experiment import FastDownwardExperiment
from downward.reports import PlanningReport
from downward.reports.absolute import AbsoluteReport
from downward.reports.scatter import ScatterPlotReport
from downward.reports.taskwise import TaskwiseReport
from lab import tools
from lab.environments import (
    BaselSlurmEnvironment,
    LocalEnvironment,
    TetralithEnvironment,
)
from lab.experiment import ARGPARSER
from lab.reports import Attribute, geometric_mean
from lab import tools


# Silence import-unused messages. Experiment scripts may use these imports.
assert (
    BaselSlurmEnvironment
    and FastDownwardExperiment
    and LocalEnvironment
    and ScatterPlotReport
    and TaskwiseReport
    and TetralithEnvironment
)


DIR = Path(__file__).resolve().parent
NODE = platform.node()
# Cover both the Basel and Linköping clusters for simplicity.
REMOTE = NODE.endswith((".scicore.unibas.ch", ".cluster.bc2.ch")) or re.match(
    r"tetralith\d+\.nsc\.liu\.se|n\d+", NODE
)


def parse_args():
    ARGPARSER.add_argument("--tex", action="store_true", help="produce LaTeX output")
    ARGPARSER.add_argument(
        "--relative", action="store_true", help="make relative scatter plots"
    )
    return ARGPARSER.parse_args()


ARGS = parse_args()
TEX = ARGS.tex
RELATIVE = ARGS.relative

EVALUATIONS_PER_TIME = Attribute(
    "evaluations_per_time", min_wins=False, function=geometric_mean, digits=1
)

# fmt: off
SUITE_STRIPS = ['agricola-strips', 'airport-strips', 'barman-strips', 'blocksworld-strips', 'childsnack-strips', 'data-network-strips', 'depots-strips', 'driverlog-strips', 'elevators-strips', 'ferry-strips', 'floortile-strips', 'freecell-strips', 'ged-strips', 'goldminer-strips', 'grid-strips', 'gripper-strips', 'hanoi-strips', 'hiking-strips', 'logistics-strips', 'miconic-strips', 'movie-strips', 'mprime-strips', 'mystery-strips', 'no-mprime-strips', 'no-mystery-strips', 'nomystery-strips', 'npuzzle-strips', 'openstacks-strips', 'organic-synthesis-split-strips', 'organic-synthesis-strips', 'parcprinter-strips', 'parking-strips', 'pathways-strips', 'pegsol-strips', 'petri-net-alignment-strips', 'pipesworld-notankage-strips', 'pipesworld-tankage-strips', 'psr-small-strips', 'rovers-strips', 'satellite-strips', 'scanalyzer-strips', 'snake-strips', 'sokoban-strips', 'spanner-strips', 'spider-strips', 'storage-strips', 'termes-strips', 'tetris-strips', 'thoughtful-strips', 'tidybot-strips', 'tpp-strips', 'transport-strips', 'trucks-strips', 'visitall-strips', 'woodworking-strips', 'zenotravel-strips']

SUITE_ADL = ['airport-adl', 'assembly-adl', 'briefcaseworld-adl', 'caldera-adl', 'caldera-split-adl', 'cavediving-adl', 'citycar-adl', 'flashfill-adl', 'fsc-adl', 'gedp-ds2ndp-adl', 'maintenance-adl', 'miconic-fulladl-adl', 'miconic-simpleadl-adl', 'nurikabe-adl', 'openstacks-adl', 'optical-telegraphs-adl', 'pathways-noneg-adl', 'philosophers-adl', 'psr-large-adl', 'psr-middle-adl', 'schedule-adl', 'settlers-adl', 't0-adl', 'trucks-adl']
# fmt: on

SUITE_IPC23_OPT = [
    "folding-opt23-adl",
    "labyrinth-opt23-adl",
    "quantum-layout-opt23-strips",
    "recharging-robots-opt23-adl",
    "ricochet-robots-opt23-adl",
    "rubiks-cube-opt23-adl",
    "slitherlink-opt23-adl",
]

SUITE_IPC23_OPT_SMALL = [
    "folding-opt23-adl:p01.pddl",
    "labyrinth-opt23-adl:p01.pddl",
    "quantum-layout-opt23-strips:p01.pddl",
    "recharging-robots-opt23-adl:p01.pddl",
    "ricochet-robots-opt23-adl:p01.pddl",
    "rubiks-cube-opt23-adl:p01.pddl",
    "slitherlink-opt23-adl:p01.pddl",
]

SUITE_IPC23_SAT_AGL = [
    "folding-sat23-adl",
    "labyrinth-sat23-adl",
    "quantum-layout-sat23-strips",
    "recharging-robots-sat23-adl",
    "ricochet-robots-sat23-adl",
    "rubiks-cube-sat23-adl",
    "slitherlink-sat23-adl",
]

SUITE_IPC23_SAT_SMALL = [
    "folding-sat23-adl:p01.pddl",
    "labyrinth-sat23-adl:p01.pddl",
    "quantum-layout-sat23-strips:p01.pddl",
    "recharging-robots-sat23-adl:p01.pddl",
    "ricochet-robots-sat23-adl:p01.pddl",
    "rubiks-cube-sat23-adl:p01.pddl",
    "slitherlink-sat23-adl:p01.pddl",
]

SUITE_STRIPS_AND_ADL = sorted(SUITE_STRIPS + SUITE_ADL)


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
    if "OPTIMAL" not in attributes:
        raise ValueError("portfolios must define OPTIMAL")
    return attributes


def prepare_config(config, replacements=None):
    replacements = [
        ("H_COST_TRANSFORM", "adapt_costs(one)"),
        ("S_COST_TYPE", "one"),
        ("BOUND", "infinity"),
        ("bound=infinity", "bound=infinity, verbosity=silent"),
    ] + (replacements or [])
    for index, part in enumerate(config):
        for before, after in replacements:
            part = part.replace(before, after)
        config[index] = part
    return config


def get_repo_base() -> Path:
    """Get base directory of the repository, as an absolute path.

    Search upwards in the directory tree from the main script until a
    directory with a subdirectory named ".git" is found.

    Abort if the repo base cannot be found."""
    path = Path(tools.get_script_path())
    while path.parent != path:
        if (path / ".git").is_dir():
            return path
        path = path.parent
    sys.exit("repo base could not be found")


def remove_file(path: Path):
    try:
        path.unlink()
    except FileNotFoundError:
        pass


def remove_properties(eval_dir: Path):
    for name in ["properties", "properties.xz"]:
        try:
            (eval_dir / name).unlink()
        except FileNotFoundError:
            pass


def compress(path: Path):
    subprocess.run(["xz", "--force", str(path)])


def add_evaluations_per_time(run):
    evaluations = run.get("evaluations")
    time = run.get("search_time")
    if evaluations is not None and evaluations >= 100 and time:
        run["evaluations_per_time"] = evaluations / time
    return run


def strip_properties(run):
    stripped_run = {}
    for attribute in [
        "id",
        "error",
        "domain",
        "problem",
        "algorithm",
        "component_options",
        "cost",
        "coverage",
        "memory",
        "run_dir",
        "total_time",
    ]:
        if attribute in run:
            stripped_run[attribute] = run[attribute]
    return stripped_run


def _get_exp_dir_relative_to_repo():
    repo_name = get_repo_base().name
    script = Path(tools.get_script_path())
    script_dir = script.parent
    rel_script_dir = script_dir.relative_to(get_repo_base())
    expname = script.stem
    return repo_name / rel_script_dir / "data" / expname


def add_scp_step(exp, login, repos_dir):
    remote_exp = Path(repos_dir) / _get_exp_dir_relative_to_repo()
    exp.add_step(
        "scp-eval-dir",
        subprocess.call,
        [
            "scp",
            "-r",  # Copy recursively.
            "-C",  # Compress files.
            f"{login}:{remote_exp}-eval",
            f"{exp.path}-eval",
        ],
    )


def fetch_algorithm(exp, expname, algo, *, new_algo=None):
    """Fetch (and possibly rename) a single algorithm from *expname*."""
    new_algo = new_algo or algo

    def rename_and_filter(run):
        if run["algorithm"] == algo:
            run["algorithm"] = new_algo
            #run["id"][0] = new_algo
            return run
        return False

    exp.add_fetcher(
        f"data/{expname}-eval",
        filter=rename_and_filter,
        name=f"fetch-{new_algo}-from-{expname}",
        merge=True,
    )


def fetch_algorithms(exp, expname, *, algos=None, name=None, filters=None):
    """
    Fetch multiple or all algorithms.
    """
    assert not expname.rstrip("/").endswith("-eval")
    algos = set(algos or [])
    filters = filters or []
    if algos:
        def algo_filter(run):
            return run["algorithm"] in algos
        filters.append(algo_filter)

    exp.add_fetcher(
        f"data/{expname}-eval",
        filter=filters,
        name=name or f"fetch-from-{expname}",
        merge=True,
    )


def add_absolute_report(exp, *, name=None, outfile=None, **kwargs):
    report = AbsoluteReport(**kwargs)
    if name and not outfile:
        outfile = f"{name}.{report.output_format}"
    elif outfile and not name:
        name = Path(outfile).name
    elif not name and not outfile:
        name = f"{exp.name}-abs"
        outfile = f"{name}.{report.output_format}"

    if not Path(outfile).is_absolute():
        outfile = Path(exp.eval_dir) / outfile

    exp.add_report(report, name=name, outfile=outfile)
    if not REMOTE:
        exp.add_step(f"open-{name}", subprocess.call, ["xdg-open", outfile])
    #exp.add_step(f"publish-{name}", subprocess.call, ["publish", outfile])


def add_scatter_plot_reports(exp, algorithm_pairs, attributes, *, filter=None):
    for algo1, algo2 in algorithm_pairs:
        for attribute in attributes:
            exp.add_report(ScatterPlotReport(
                    relative=RELATIVE,
                    get_category=None if TEX else lambda run1, run2: run1["domain"],
                    attributes=[attribute],
                    filter_algorithm=[algo1, algo2],
                    filter=[add_evaluations_per_time, group_domains] + tools.make_list(filter),
                    format="tex" if TEX else "png",
                ),
                name=f"{exp.name}-{algo1}-{algo2}-{attribute}{'-relative' if RELATIVE else ''}")


class QualityFilters:
    """Compute the IPC quality score.

    >>> from downward.reports.absolute import AbsoluteReport
    >>> filters = QualityFilters()
    >>> report = AbsoluteReport(filter=[filters.store_costs, filters.add_quality])

    """

    def __init__(self):
        self.tasks_to_costs = defaultdict(list)

    def _get_task(self, run):
        return (run["domain"], run["problem"])

    def _compute_quality(self, cost, all_costs):
        if cost is None:
            return 0.0
        assert all_costs
        min_cost = min(all_costs)
        if cost == 0:
            assert min_cost == 0
            return 1.0
        return min_cost / cost

    def store_costs(self, run):
        cost = run.get("cost")
        if cost is not None:
            assert run["coverage"]
            self.tasks_to_costs[self._get_task(run)].append(cost)
        return True

    def add_quality(self, run):
        run["quality"] = self._compute_quality(
            run.get("cost"), self.tasks_to_costs[self._get_task(run)]
        )
        return run


class Hardest30Report(PlanningReport):
    """
    Keep the 30 tasks from each domain that are solved by the fewest number of planners.
    """
    def get_text(self):
        solved_by = defaultdict(int)
        for run in self.props.values():
            if run.get("coverage"):
                solved_by[(run["domain"], run["problem"])] += 1
        hardest_tasks = {}
        for domain, problems in sorted(self.domains.items()):
            solved_problems = [problem for problem in problems if solved_by[(domain, problem)] > 0]
            solved_problems.sort(key=lambda problem: solved_by[(domain, problem)])
            hardest_tasks[domain] = set(solved_problems[:30])
        for domain, problems in sorted(self.domains.items()):
            print(domain, len(problems), len(hardest_tasks[domain]))
        new_props = tools.Properties()
        for key, run in self.props.items():
            if run["problem"] in hardest_tasks[run["domain"]]:
                new_props[key] = run
        return str(new_props)
