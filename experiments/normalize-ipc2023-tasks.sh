#! /bin/bash

set -euo pipefail

for file in {sat,opt}/{folding,recharging-robots,rubiks-cube,slitherlink}/p*.pddl; do
    dir=$(dirname "$file")
    name=$(basename "$file")

    mkdir -p "normalized/$dir"
    ./ipc23-normalize.sif "$dir/domain.pddl" "$file" "normalized/$dir/domain_$name" "normalized/$dir/$name"
done
