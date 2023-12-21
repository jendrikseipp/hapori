#! /bin/bash

# This script executes validate on either the the given plan file
# <sasplan> or the plan file <sasplan.x>, where x is the largest value
# such that <sasplan.k> exists for all 1 <= k <= x.

if [ "$#" -ne 3 ]; then
    echo "Must specify domain file, problem file and (base name) of
    plan file on which validate will be run if it exists."
    exit 2
fi

if [[ -f "$3" ]]; then
    echo "running validate on $3"
    validate "$@"
elif [[ -f "$3.1" ]]; then
    x=1
    while [[ -f "$3.$(($x + 1))" ]]
    do
      x=$(($x + 1))
    done
    echo "running validate on $3.$x"
    validate "$1" "$2" "$3.$x"
else
    echo "no plan file(s) found; not running validate"
fi

