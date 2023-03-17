#! /bin/bash

set -euo pipefail

OPT_DATA=../../experiments/data/01-opt-planners-eval/properties-hardest.json.xz

#./batch-stonesoup.sh ${OPT_DATA} opt 1800 | tee batch-stonesoup-opt.txt
#./stonesoup.py --track opt ${OPT_DATA} 110 | tee stonesoup-opt.txt

./greedy.py ${OPT_DATA} --track opt | tee greedy-opt.txt
