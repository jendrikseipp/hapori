import numpy

import imp
try:
    libselector = imp.load_dynamic('libselector', 'config_selector/libselector.so')
except ImportError as err:
    import os
    import sys
    sys.exit(f'Error: {err}. Please follow the instructions in '
             f'{os.path.dirname(os.path.abspath(__file__))}/README to compile '
             f'the Selector')


def max_subset(timetable, subset_size):
    """
    Same as min_subset but with max instead of min and no slow implementation.
    """
    if not isinstance(timetable, numpy.ndarray):
        # try to convert timetable
        timetable = numpy.array(timetable)
    # subset_size greater than number of configs
    if subset_size > timetable.shape[1]:
        raise ValueError("subset_size greater than number of configs")

    return libselector.choose_max_subset(timetable, subset_size)


def min_subset(timetable, subset_size, slow=False):
    """
    Calculates  the subset of size subset_size which minimizes
    sum_problems(min_configs(timetable[config, problem]))
    timetable is a numpy.ndarray. Rows (1st dimension) correspond
    to problems, columns (2nd dimension to configs)
    timetable can be given as a numpy.ndarray or list of lists.

    if slow is set to True, it uses a slow, pure-python implementation
    otherwise a faster c++ method, which has to be compiled.
    to compile it, run cmake . && make in the module directory

    returns the value of the objective function and the list of indices for
    the choosen subset:
    (min_value, [idx1, idx2 ...])
    """
    if not isinstance(timetable, numpy.ndarray):
        # try to convert timetable
        timetable = numpy.array(timetable)
    # subset_size greater than number of configs
    if subset_size > timetable.shape[1]:
        raise ValueError("subset_size greater than number of configs")
    if slow:
        # use pure python implementation
        import slow
        slow.best_subset(timetable, subset_size)
        return
    else:
        return libselector.choose_min_subset(timetable, subset_size)
