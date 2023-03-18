#! /bin/bash

set -euo pipefail

cd $(dirname "$0")

TMPDIR="/tmp/apptainer-rundir"

RECIPE=$(realpath ${1})
IMAGE=$(realpath ${2})
BENCHMARKS_DIR=benchmarks


printf "\n\n**********************************************************************\n\n\n"
echo "Recipe: ${RECIPE}"
if [[ -e ${IMAGE} ]]; then
    echo "Image ${IMAGE} exists -> will test it now."
else
    echo "Image ${IMAGE} does not exist -> will create and test it now."
    pushd $(dirname ${RECIPE})
    apptainer build ${IMAGE} ${RECIPE}
    popd
fi

echo "Testing image at ${IMAGE}:"
rm -rf ${TMPDIR}
mkdir ${TMPDIR}
cp ${BENCHMARKS_DIR}/miconic-strips/domain-2-s1-0.pddl ${TMPDIR}/domain.pddl
cp ${BENCHMARKS_DIR}/miconic-strips/2-s1-0.pddl ${TMPDIR}/problem.pddl
DOMAIN="${TMPDIR}/domain.pddl"
PROBLEM="${TMPDIR}/problem.pddl"
PLANFILE="${TMPDIR}/my_sas_plan"

${IMAGE} ${DOMAIN} ${PROBLEM} ${PLANFILE}
