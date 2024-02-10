#! /bin/bash

set -euo pipefail

cd "$(dirname "$0")"

function learn {
    track=${1}
    echo "Learning ${track} portfolios"
    ./learn-portfolios-for-track.py --track ${track} --all | tee learning-logs-${track}.txt
    ./analyze-learning-results.sh ${track} | tee --append learning-logs-${track}.txt
}

learn opt
learn sat
learn agl
