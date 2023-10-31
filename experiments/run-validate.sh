#! /bin/bash

if [ "$#" -ne 3 ]; then
    echo "Must specify domain file, problem file and plan file on which
    validate will be run if it exists."
    exit 2
fi

if [[ -f "$3" ]]; then
    validate "$@"
fi

