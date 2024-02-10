#! /bin/bash

set -euo pipefail

cd "$(dirname "$0")"

# Turn to lowercase.
TRACK=${1,,}

PORTFOLIOS_DIR=../../sequential-portfolios/

function analyze {
    name=${1}
    echo ${name}
    grep --with-filename Score data/learn-${TRACK}-portfolios-eval/${name}*.py | sort -n -k 2 | tee ${name}-sorted.txt
    grep --with-filename "Time for computing portfolio" data/learn-${TRACK}-portfolios-eval/${name}*.py | tee ${name}-times.txt

    # Split last line of output by colon and store the first part.
    BEST_PORTFOLIO=$(tail -n 1 ${name}-sorted.txt | cut -d ':' -f 1)
    echo Best ${name} portfolio: $BEST_PORTFOLIO
    cp ${BEST_PORTFOLIO} ${PORTFOLIOS_DIR}/hapori-${name}-${TRACK}.py

    echo "Total time for computing best portfolio:" `awk '/Time for computing portfolio:/ { sum += $5 } END { print sum }' ${name}-times.txt`

    rm ${name}-sorted.txt ${name}-times.txt
    echo
}

analyze uniform
#analyze selector
analyze cluster
analyze increasing-time-limit
analyze domain-wise
analyze randomized-iterative-search
