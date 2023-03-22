import enum
from typing import Tuple

import numpy as np


class FeatureTransformations(enum.Enum):
    NONE = "none"
    LOG = "log"
    NORMALIZE = "normalize"
    # LOG_NORMALIZE = "log_normalize"


def _get_normalize_args(x: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
    """
    Calculate for a feature array for each feature the min and (max-min)
    value s.t. the feature can be normalized (by (f - min)/(max-min)).
    :param x: feature array (TxF)
    :return: [min for each feature], [max - min for each feature]
    """
    arg_min = np.min(x, axis=0)
    arg_div = np.max(x, axis=0) - arg_min
    arg_div[arg_div == 0] = 1
    return arg_min, arg_div


def _get_log_args(x: np.ndarray) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Checks for every feature if its values are only positive or negative
    :param x: feature array (TxF)
    :return: mask only negative, mask only positive, mask either pos. or neg.
    """
    neg = np.sum(x < 0, axis=0) == x.shape[0]
    pos = np.sum(x > 0, axis=0) == x.shape[0]
    either = np.logical_or(neg, pos)
    return neg, pos, either


# def _get_log_normalize_args(x: np.ndarray) -> Tuple[
#     Tuple[np.ndarray, np.ndarray, np.ndarray], Tuple[np.ndarray, np.ndarray]]:
#     """
#     Calculate both the log and the normalization arguments.
#     TODO: This is wrong, isn't it? normalization after log
#     :param x: feature array (TxF)
#     :return: log transformation args, normalization transformation args
#     """
#     return _get_log_args(x), _get_normalize_args(x)


def _feature_adapt_log(
        x: np.ndarray,
        _args: Tuple[np.ndarray, np.ndarray, np.ndarray]) -> np.ndarray:
    """
    Transforms the given features by taking the log (if the feature has only
    positive or negative values)
    :param x: feature matrix (TxF)
    :param _args: feature transformation arguments (see _get_log_args)
    :return: new array with transformed features
    """
    x = x.copy()
    neg, pos, either = _args
    x[:, neg] = np.log(-x[:, neg])
    x[:, pos] = np.log(x[:, pos])
    return x[:, either]


# def _feature_adopt_log_normalize(x, _args):
#     x = _feature_adapt_log(x, _args[0])
#     min_value, divisor = [a[_args[0][2]] for a in _args[1]]
#     return (x - min_value) / divisor


feature_transformations_args = {
    FeatureTransformations.NONE: lambda _x: [],
    FeatureTransformations.LOG: _get_log_args,
    FeatureTransformations.NORMALIZE: _get_normalize_args,
    # FeatureTransformations.LOG_NORMALIZE: _get_log_normalize_args,
}

feature_transformations = {
    FeatureTransformations.NONE: lambda _x, _args: _x,
    FeatureTransformations.LOG: _feature_adapt_log,
    FeatureTransformations.NORMALIZE: lambda _x, _args: (
        (_x.astype(float) - _args[0]) / _args[1]),
    # FeatureTransformations.LOG_NORMALIZE: _feature_adopt_log_normalize,
}
assert all(_x in feature_transformations for _x in FeatureTransformations)

if __name__ == "__main__":
    print("Thou shall not call me directly.")
