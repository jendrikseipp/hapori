import argparse
import enum
from typing import Callable, List, Set, Tuple

import numpy as np

from .constants import R


class Splits(enum.Enum):
    RANDOM = "random"  # randomly split the task
    DOMAIN_PRESERVING = "domain_preserving"  # key tasks of same domain together
    PREDEFINED = "predefined"  # use a predefined task split
    TRAINING = "training"


def load_split_file(file_name: str) -> Set[str]:
    """
    Load tasks belonging to one split
    :param file_name: path to the file describing the split
    :return: set of tasks in the split
    """
    tasks = set()
    with open(file_name, "r") as f:
        lines = [x.strip() for x in f.readlines() if x.strip() != ""]
        for line in lines:
            line_split = [x for x in line.split(" ") if x.strip() != ""]
            assert len(line_split) == 2, "{}: {}".format(file_name, line)
            tasks.add(line_split[1])
    return tasks


def load_split_files(options: argparse.Namespace) -> List[Set[str]]:
    """
    Load all splits for the tasks into sets
    :param options: main command line arguments
    :return: for each specified split file the tasks described in it
    """
    return [load_split_file(x) for x in options.split_files]


def get_mask_for_split(problem_names: np.ndarray, split: Set[str]
                       ) -> np.ndarray:
    """
    Create a mask telling for every task if the task belongs to the given split
    :param problem_names: list of all tasks (as given in the data arrays)
    :param split: set of tasks in the split
    :return: bool numpy array masking the tasks in the split
    """
    return np.array([p in split for p in problem_names])


def split_training(options: argparse.Namespace,
                     problem_names: np.ndarray
                     ) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Split the tasks into training, validation, test data as defined by the
    provided split files.
    :param options: main command line arguments
    :param problem_names: list of tasks as given in the data arrays
    :return: boolean mask for training, validation and test tasks.
    """
    assert options.cross_validate is None
    true_mask = [True for _ in problem_names]
    false_mask = [False for _ in problem_names]
    return true_mask, false_mask, true_mask


def split_predefined(options: argparse.Namespace,
                     problem_names: np.ndarray
                     ) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Split the tasks into training, validation, test data as defined by the
    provided split files.
    :param options: main command line arguments
    :param problem_names: list of tasks as given in the data arrays
    :return: boolean mask for training, validation and test tasks.
    """
    assert options.cross_validate is None
    predefined = load_split_files(options)
    split = [get_mask_for_split(problem_names, split) for split in predefined]
    assert len(split) == 3, "see return line"
    return split[0], split[1], split[2]  # what we do for type checking...


def split_func(func: Callable[[argparse.Namespace, List[int], Set[str]],
                              List[np.ndarray]]
               ) -> Callable[[argparse.Namespace, np.ndarray],
                             Tuple[np.ndarray, np.ndarray, np.ndarray]]:
    """
    Split the tasks into training, validation, test data. How the tasks
    are distributed is defined by the given function.
    :param func: function to map tasks to data sets
    :return: boolean masks for training, validation and test data
    """
    def _split(options: argparse.Namespace, problem_names: np.ndarray
               ) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        assert (options.cross_validate is not None
                and options.cross_validate >= 0)
        predefined = load_split_files(options)

        train, valid, test = None, None, None
        to_distribute = predefined[0] | predefined[1]
        if options.split_exclude_test:
            test = predefined[2]
            folds = [options.cross_validate_folds - 1, 1]
        else:
            to_distribute |= predefined[2]
            folds = [options.cross_validate_folds - 2, 1, 1]
        assert sum(folds) == options.cross_validate_folds, folds
        assert 2 <= len(folds) <= 3

        distributed = func(options, folds, to_distribute)

        if options.split_exclude_test:
            train, valid = distributed
        else:
            train, valid, test = distributed
        split = [get_mask_for_split(problem_names, split)
                 for split in [train, valid, test]]
        assert len(split) == 3, "see next line"
        return split[0], split[1], split[2]  # what we do for type checking
    return _split


def _split_random(options: argparse.Namespace, folds: List[int],
                  to_distribute: Set[str]) -> List[np.ndarray]:
    """
    Internal function to split tasks randomly into sets
    :param options: main command line options
    :param folds: number of folds for each set
    :param to_distribute: tasks to distribute
    :return: for each set an np.array containing its tasks
    """
    nb_folds = options.cross_validate_folds
    cvi = options.cross_validate
    assert cvi < nb_folds
    assert sum(folds) == nb_folds

    shift = 0
    assigned_folds = []
    for fold in folds:
        assigned_folds.append([(i + shift + options.cross_validate) % nb_folds
                               for i in range(fold)])
        shift += fold

    list_to_distribute = sorted(list(to_distribute))
    R.shuffle(list_to_distribute)

    fold_size = int(len(list_to_distribute) / nb_folds)

    def _get_start_end(_fold: int) -> Tuple[int, int]:
        assert _fold < nb_folds
        return _fold * fold_size, (_fold + 1) * fold_size

    distributed: List[List[List[str]]] = []
    for af in assigned_folds:
        distributed.append([])
        for fold in af:
            start, end = _get_start_end(fold)
            distributed[-1].append(list_to_distribute[start:end])
    distributed[0].append(list_to_distribute[nb_folds * fold_size:])

    return [np.concatenate(d) for d in distributed]


def _split_domain(options: argparse.Namespace, folds: List[int],
                  to_distribute: Set[str]) -> List[np.ndarray]:
    """
    Internal function to split tasks randomly, but domain preserving into sets.
    :param options: main command line options
    :param folds: number of folds for each set
    :param to_distribute: tasks to distribute
    :return: for each set an np.array containing its tasks
    """
    nb_folds = options.cross_validate_folds
    cvi = options.cross_validate
    assert cvi < nb_folds
    assert sum(folds) == nb_folds

    with open(options.domain_groups, "r") as f:
        groups = set([x.strip() for x in f.readlines() if x.strip() != ""])
    distributed_groups = _split_random(options, folds, groups)
    distributed_tasks = [np.array([t for t in to_distribute
                         if any(t.startswith(dg) for dg in dgs)])
                         for dgs in distributed_groups]
    return distributed_tasks


splits = {
    Splits.PREDEFINED: split_predefined,
    Splits.RANDOM: split_func(_split_random),
    Splits.DOMAIN_PRESERVING: split_func(_split_domain),
    Splits.TRAINING: split_training,

}
assert all(_x in splits for _x in Splits)


if __name__ == "__main__":
    print("Thou shall not call me directly.")
