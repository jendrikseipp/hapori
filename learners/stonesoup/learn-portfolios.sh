#! /bin/bash

set -euo pipefail

OPT_DATA=../../experiments/data/01-opt-planners-eval/properties-hardest.json.xz

./batch-stonesoup.sh ${OPT_DATA} opt 1800 | tee batch-stonesoup-opt.txt
./stonesoup.py --track opt ${OPT_DATA} 300 | tee stonesoup-opt.txt
./greedy.py ${OPT_DATA} --track opt | tee greedy-opt.txt


SAT_DATA=../../experiments/data/02-sat-planners-eval/properties-hardest.json.xz

./batch-stonesoup.sh ${SAT_DATA} sat 1800 | tee batch-stonesoup-sat.txt
./stonesoup.py --track sat ${SAT_DATA} 40 | tee stonesoup-sat.txt
./greedy.py ${SAT_DATA} --track sat | tee greedy-sat.txt


./greedy.py ${SAT_DATA} --track agl | tee greedy-agl.txt
