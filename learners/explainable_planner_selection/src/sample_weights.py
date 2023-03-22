import enum
from typing import List, Optional, Tuple

import numpy as np

KEYWORD = "sample_weight"


class Weights(enum.Enum):
    CUSTOM = "custom"
    CLASS_BALANCED = "classBalanced"
    SKIP_TRIVIALLY_SOLVED = "skipTriviallySolved"


def parse_custom(y_time: np.ndarray, args: List[str]):
    params = []  # Shift, Divisor, Exponent
    for no, default in enumerate([0, 1, 1]):
        params.append(default if len(args) <= no else eval(args[no].format(nb_planners=y_time.shape[1])))
    return params


def weight_custom(
        args: List[str],
        timeout: int,
        y_train_time: np.ndarray
) -> Tuple[Optional[np.ndarray], Optional[np.ndarray]]:
    shift, divisor, exponent = parse_custom(y_train_time, args)
    nb_planners = y_train_time.shape[1]

    cov = (y_train_time < timeout).sum(axis=1)
    return np.power((nb_planners - cov + shift)/divisor, exponent)


def weight_class_balanced(
        args: List[str],
        timeout: int,
        y_train_time: np.ndarray
) -> Tuple[Optional[np.ndarray], Optional[np.ndarray]]:
    assert len(args) == 0
    cov = (y_train_time <= timeout).sum(axis=1)
    clazz, counts = np.unique(cov, return_counts=True)
    weights = np.zeros(shape=(len(y_train_time)))
    for cl, co in zip(*[clazz, counts]):
        weights[cov == cl] = 1.0/co
    return weights


def weight_trivially_solvable(
        args: List[str],
        timeout: int,
        y_train_time: np.ndarray) -> Tuple[Optional[np.ndarray], Optional[np.ndarray]]:
    assert len(args) == 0
    cov = (y_train_time <= timeout).sum(axis=1)
    weights = np.ones(shape=len(y_train_time))
    weights[cov == y_train_time.shape[1]] = 0
    return weights


func_weighting = {
    Weights.CUSTOM.value.lower(): weight_custom,
    Weights.CLASS_BALANCED.value.lower(): weight_class_balanced,
    Weights.SKIP_TRIVIALLY_SOLVED.value.lower(): weight_trivially_solvable,
}


def calculate(
        args: Optional[List[str]],
        timeout: int,
        y_train_time: np.ndarray) -> Tuple[Optional[np.ndarray], Optional[np.ndarray]]:
    if args is None:
        return None
    assert len(args) > 0
    assert args[0].lower() in func_weighting.keys(), args[0]
    return func_weighting[args[0].lower()](args[1:], timeout, y_train_time)
