import itertools


def best_subset(timetable, subset_size):
    num_problems, num_configs = timetable.shape
    prob_indices_range = range(num_problems)

    def prob_iterator(subset_indices):
        """
        yields the best (minimum) of configuration subset for all problems
        """
        # copy to list as we use it multiple times
        subset_indices = list(subset_indices)
        for row in prob_indices_range:
            yield min(timetable[row, col] for col in subset_indices)

    def objective_fn(subset_indices):
        """ the objective function we minimize"""
        return sum(prob_iterator(subset_indices))

    config_indices = range(num_configs)
    # all subsets of size subset_size
    subsets_iter = itertools.combinations(config_indices, subset_size)
    best_subset = min(subsets_iter, key=objective_fn)
    return objective_fn(best_subset), best_subset
