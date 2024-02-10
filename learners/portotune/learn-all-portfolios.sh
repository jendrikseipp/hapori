#! /bin/bash

set -euo pipefail

cd "$(dirname "$0")"

./learn-opt-portfolios.py --all | tee learning-logs-opt.txt
./analyze-learning-results.sh opt | tee --append learning-logs-opt.txt
