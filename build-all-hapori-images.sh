#! /bin/bash

set -euo pipefail

cd $(dirname "$0")

OUTDIR=.
mkdir -p ${OUTDIR}

for recipe in Apptainer.* ; do
    name="${recipe##*.}"
    ./build-hapori-image.sh ${recipe} ${OUTDIR}/${name}.img
done

echo "Finished building images"
