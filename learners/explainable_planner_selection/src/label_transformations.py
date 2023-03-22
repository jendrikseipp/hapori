import enum

import numpy as np


class LabelTransformations(enum.Enum):
    NONE = "none"
    TIME = "time"
    BINARY = "binary"
    LOG = "log"
    # LOG_NORMALIZE = "log_normalize"


# def _get_normalize_args(x: np.ndarray) -> Tuple[]:
#     arg_min = np.min(x, axis=0)
#     arg_div = np.max(x, axis=0) - arg_min
#     arg_div[arg_div == 0] = 1
#     return [arg_min, arg_div]

#
# label_transformation_args = {
#     LabelTransformations.NONE: lambda _x: [],
#     LabelTransformations.TIME: lambda _x: [],
#     LabelTransformations.BINARY: lambda _x: [],
#     LabelTransformations.LOG: lambda _x: [],
#     # LabelTransformations.LOG_NORMALIZE: _get_normalize_args
# }


label_transformations = {
    LabelTransformations.NONE: lambda _x, _options: _x,
    LabelTransformations.TIME: lambda _x, _options: _x,
    LabelTransformations.BINARY: lambda _x, _options: (
            _x < _options.timeout).astype(int),
    LabelTransformations.LOG: lambda _x, _options: np.log(_x),
    # LabelTransformations.LOG_NORMALIZE: lambda _x, _args, _options:
    #     (np.log(_x) - _args[0]) / _args[1]
}

assert all(_x in label_transformations for _x in LabelTransformations)

choose_max_label = {
    LabelTransformations.NONE: False,
    LabelTransformations.TIME: False,
    LabelTransformations.LOG: False,
    LabelTransformations.BINARY: True
}

assert all(_x in choose_max_label for _x in LabelTransformations)

if __name__ == "__main__":
    print("Thou shall not call me directly.")
