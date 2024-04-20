#! /bin/bash

set -euo pipefail

for exp in {evaluation-on-trainingset-opt,evaluation-on-testset-opt,evaluation-on-trainingset-sat,evaluation-on-testset-sat}; do
    ./2024-03-29-${exp}.py 5
done

tar czvf reports.tar.gz data/2024-03-29-evaluation-on-*-eval/2024-03-29-evaluation-on-*.html
