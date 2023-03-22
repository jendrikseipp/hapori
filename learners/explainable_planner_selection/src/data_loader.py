import argparse
import sys
from typing import Tuple, List, Optional, Pattern

import numpy as np

from .data_splits import splits
from .feature_transformations import (
    FeatureTransformations, feature_transformations,
    feature_transformations_args)
from .label_transformations import label_transformations


def load_filter(path: Optional[str]) -> Optional[List[str]]:
    if path is None:
        return None
    with open(path, "r") as f:
        elems = [x.strip() for x in f.readlines() if x.strip() != ""]
        return [e.split(" ")[1] if e.split(" ") == 2 else e for e in elems]


def get_white_black_list(elems, file_filter, list_filter, regex_filter):
    if all(f is None for f in [file_filter, list_filter, regex_filter]):
        return None
    feature_filter = set()
    if file_filter is not None:
        feature_filter.update(load_filter(file_filter))
    if list_filter is not None:
        feature_filter.update(list_filter)
    if regex_filter is not None:
        feature_filter.update(
            [f for f in elems if regex_filter.match(f) is not None])
    return feature_filter


def get_mask(elems, whitelist, blacklist):
    unused = (
        [w for w in ([] if whitelist is None else whitelist)
         if w not in elems] +
        [b for b in ([] if blacklist is None else blacklist)
         if b not in elems])
    assert len(unused) == 0, unused
    return [
        (whitelist is None or e in whitelist) and
        (blacklist is None or e not in blacklist)
        for e in elems]


def load_features(
        path: str,
        file_feature_whitelist: Optional[str] = None,
        list_feature_whitelist: Optional[List[str]] = None,
        regex_feature_whitelist: Optional[Pattern] = None,
        file_feature_blacklist: Optional[str] = None,
        list_feature_blacklist: Optional[List[str]] = None,
        regex_feature_blacklist: Optional[Pattern] = None,
        file_task_whitelist: Optional[str] = None,
) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
    ary = np.loadtxt(path, dtype=object, delimiter=";")
    feature_names = ary[0, 1:]
    problem_names = ary[1:, 0]
    feature_values = ary[1:, 1:].astype(float)

    feature_whitelist = get_white_black_list(
        feature_names,
        file_feature_whitelist, list_feature_whitelist, regex_feature_whitelist)
    feature_blacklist = get_white_black_list(
        feature_names,
        file_feature_blacklist, list_feature_blacklist, regex_feature_blacklist)
    feature_mask = get_mask(feature_names, feature_whitelist, feature_blacklist)

    task_whitelist = get_white_black_list(
        problem_names,
        file_task_whitelist, None, None)
    task_mask = get_mask(problem_names, task_whitelist, None)

    problem_names = problem_names[task_mask]
    feature_names = feature_names[feature_mask]
    feature_values = feature_values[:, feature_mask][task_mask, :]
    problem_names = np.array([p[:-5] if p.endswith(".pddl") else p for p in problem_names])
    return problem_names, feature_names, feature_values


def load_runtimes(
        path: str,
        file_planner_whitelist: Optional[str],
        file_task_whitelist: Optional[str]
) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
    ary = np.loadtxt(path, dtype=object, delimiter=",")
    ary[ary == "-"] = 10000
    planner_names = ary[0, 1:]
    problem_names = np.array(list(map(
        lambda x: x[:-5] if x.endswith(".pddl") else x,
        ary[1:, 0])))
    runtimes = ary[1:, 1:].astype(float)

    planner_whitelist = get_white_black_list(
        planner_names, None, None, None)
    planner_mask = get_mask(planner_names, planner_whitelist, None)

    task_whitelist = get_white_black_list(
        problem_names, file_task_whitelist, None, None)
    task_mask = get_mask(problem_names, task_whitelist, None)

    planner_names = planner_names[planner_mask]
    runtimes = runtimes[:, planner_mask][task_mask]
    problem_names = problem_names[task_mask]
    return problem_names, planner_names, runtimes


def select_subset(keys_sub, keys_sup, data):
    set_sub = set(keys_sub)
    mask = [k in set_sub for k in keys_sup]
    keys_sup = keys_sup[mask]
    data = data[mask]
    return keys_sup, data


def reorder_ary(keys: np.ndarray, ary: np.ndarray, axis: int = 1
                ) -> Tuple[np.ndarray, np.ndarray]:
    assert axis == 1, "Other axis are not supported"
    idx_sort = np.argsort(keys)
    keys = keys[idx_sort]
    ary = ary[idx_sort]
    return keys, ary


def load_data(options: argparse.Namespace,
              return_names=False
              ) -> Tuple[Tuple[np.ndarray, np.ndarray, np.ndarray],
                         Tuple[np.ndarray, np.ndarray, np.ndarray],
                         Tuple[np.ndarray, np.ndarray, np.ndarray]]:
    # Load features and runtimes
    raw_data = []
    task_names, feature_names, feature_values = load_features(
        path=options.features[0],
        file_feature_whitelist=options.file_feature_whitelist,
        regex_feature_whitelist=options.regex_feature_whitelist,
        list_feature_blacklist=options.list_feature_blacklist,
        regex_feature_blacklist=options.regex_feature_blacklist,
        file_task_whitelist=options.file_task_whitelist
    )

    task_names2, planner_names, runtimes = load_runtimes(
        path=options.runtimes,
        file_planner_whitelist=options.file_planner_whitelist,
        file_task_whitelist=options.file_task_whitelist
    )
    assert set(task_names).issubset(set(task_names2)), task_names
    task_names2, runtimes = select_subset(
        task_names, task_names2, runtimes)
    assert set(task_names) == set(task_names2)

    task_names, feature_values = reorder_ary(task_names, feature_values)
    task_names2, runtimes = reorder_ary(task_names2, runtimes)
    assert(np.all(task_names == task_names2))
    raw_data.append(feature_values)
    raw_data.append(runtimes)
    raw_data.append(runtimes)

    # Split the tasks into train/valid/test set
    split_masks = splits[options.split](options, task_names)
    assert all(sum(m[i] for m in split_masks) <= 3
               for i in range(len(split_masks[0])))
    if options.only_print_task_sets:
        print()
        for name, mask in zip(["train", "valid", "test"], split_masks):
            print("@{}:{}".format(name, ";".join(task_names[mask])))
        sys.exit(0)
    data = [[rd[mask] for rd in raw_data] for mask in split_masks]
    task_names_split = [task_names[mask] for mask in split_masks]

    # Prepare data transformations
    data_transformers = []
    ft_args = [feature_transformations_args[ft](data[0][0])
               for ft in options.feature_transformation]
    data_transformers.append(lambda x: np.concatenate([
        feature_transformations[ft](x, ft_args[no])
        for no, ft in enumerate(options.feature_transformation)],
        axis=1),)

    data_transformers.append(
        lambda y:
        label_transformations[options.label_transformation](y, options))
    data_transformers.append(lambda y: y)
    assert len(raw_data) == len(data_transformers)

    # Print features names
    fn = []
    for ft, args in zip(options.feature_transformation, ft_args):
        if ft == FeatureTransformations.NONE:
            fn.extend(["\"%s\"" % x for x in feature_names])
        elif ft == FeatureTransformations.LOG:
            fn.extend("\"log(%s)\"" % x for x in feature_names[args[2]])
        elif ft == FeatureTransformations.NORMALIZE:
            fn.extend(["\"norm(%s)\"" % x for x in feature_names])
        else:
            assert False
    print("Feature names ({}):".format(len(fn)))
    print(", ".join(fn))

    # Transform raw data to final data
    train_valid_test = [(
        t(rd) for rd, t in zip(data_set, data_transformers))
        for data_set in data]
    assert len(train_valid_test) == 3, "what we do for typing"
    retval = [train_valid_test[0], train_valid_test[1], train_valid_test[2]]
    if return_names:
        retval.append(task_names_split)
        retval.append(planner_names)
        retval.append(feature_names)
    return retval


if __name__ == "__main__":
    print("Thou shall not call me directly.")
