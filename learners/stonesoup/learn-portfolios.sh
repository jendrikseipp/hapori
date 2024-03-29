#! /bin/bash

set -euo pipefail

cd "$(dirname "$0")"

OPT_DATA=../../experiments/data/training-data-collect-eval/properties-hardest-opt.json.xz
SAT_DATA=../../experiments/data/training-data-collect-eval/properties-hardest-sat.json.xz

# Run Stone Soup for different granularity values.
./batch-stonesoup.sh ${OPT_DATA} opt 1800 | tee batch-stonesoup-opt.txt
# Pick best granularity value and run Stone Soup again.
./stonesoup.py --track opt ${OPT_DATA} 300 | tee stonesoup-opt.txt
./greedy.py ${OPT_DATA} --track opt | tee greedy-opt.txt

# Run Stone Soup for different granularity values.
./batch-stonesoup.sh ${SAT_DATA} sat 1800 | tee batch-stonesoup-sat.txt
# Pick best granularity value and run Stone Soup again.
./stonesoup.py --track sat ${SAT_DATA} 30 | tee stonesoup-sat.txt
./greedy.py ${SAT_DATA} --track sat | tee greedy-sat.txt
