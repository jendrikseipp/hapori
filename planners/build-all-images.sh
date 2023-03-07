#! /bin/bash

set -euo pipefail

cd $(dirname "$0")

OUTDIR=../images/

# OPT+SAT+AGL
./build-image.sh ../planners/ipc2018-decstar/Singularity ${OUTDIR}/ipc2018-decstar.img || true
./build-image.sh ../planners/ipc2018-symple1/Singularity ${OUTDIR}/ipc2018-symple1.img
./build-image.sh ../planners/ipc2018-symple2/Singularity ${OUTDIR}/ipc2018-symple2.img

# SAT+AGL
./build-image.sh ../planners/ipc2018-saarplan/Singularity ${OUTDIR}/ipc2018-saarplan.img || true

# OPT
./build-image.sh ../planners/ipc2014-opt-symba/Singularity-01 ${OUTDIR}/ipc2014-opt-symba1.img
./build-image.sh ../planners/ipc2018-opt-complementary1/Singularity ${OUTDIR}/ipc2018-opt-complementary1.img
./build-image.sh ../planners/ipc2018-opt-complementary2/Singularity ${OUTDIR}/ipc2018-opt-complementary2.img
./build-image.sh ../planners/ipc2018-opt-delfi/Apptainer ${OUTDIR}/ipc2018-opt-delfi.img
./build-image.sh ../planners/ipc2018-opt-metis/Singularity ${OUTDIR}/ipc2018-opt-metis.img
./build-image.sh ../planners/ipc2018-opt-planning-pdbs/Singularity ${OUTDIR}/ipc2018-opt-planning-pdbs.img
./build-image.sh ../planners/ipc2018-opt-scorpion/Singularity ${OUTDIR}/ipc2018-opt-scorpion.img


# AGL+SAT
./build-image.sh ../planners/ipc2014-agl-jasper/Singularity ${OUTDIR}/ipc2014-agl-jasper.img
./build-image.sh ../planners/ipc2014-agl-madagascar/Singularity-MpC ${OUTDIR}/ipc2014-agl-mpc.img
./build-image.sh ../planners/ipc2014-agl-probe/Singularity ${OUTDIR}/ipc2014-agl-probe.img
./build-image.sh ../planners/ipc2018-agl-cerberus/Singularity ${OUTDIR}/ipc2018-agl-cerberus.img  || true  # finds no plan?
./build-image.sh ../planners/ipc2018-agl-decstar/Singularity ${OUTDIR}/ipc2018-agl-decstar.img
./build-image.sh ../planners/ipc2018-agl-fdss-2018/Singularity ${OUTDIR}/ipc2018-fd-2018.img
./build-image.sh ../planners/ipc2018-agl-lapkt-dual-bfws/Apptainer ${OUTDIR}/ipc2018-agl-lapkt-bfws.img
./build-image.sh ../planners/ipc2018-agl-lapkt-dfs-plus/Singularity ${OUTDIR}/ipc2018-agl-lapkt-dfs-plus.img
./build-image.sh ../planners/ipc2018-agl-mercury2014/Singularity ${OUTDIR}/ipc2018-agl-mercury2014.img  || true  # finds no plan?
./build-image.sh ../planners/ipc2018-agl-merwin/Singularity ${OUTDIR}/ipc2018-agl-merwin.img || true  # finds no plan?
./build-image.sh ../planners/ipc2018-agl-olcff/Singularity ${OUTDIR}/ipc2018-agl-olcff.img

echo "Finished building images"
