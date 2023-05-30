#! /bin/bash

set -euo pipefail

cd $(dirname "$0")

OUTDIR=../images/

# OPT+SAT+AGL
./build-image.sh ../planners/ipc2018-decstar/Apptainer ${OUTDIR}/ipc2018-decstar.img || true
./build-image.sh ../planners/ipc2018-symple1/Apptainer ${OUTDIR}/ipc2018-symple1.img
./build-image.sh ../planners/ipc2018-symple2/Apptainer ${OUTDIR}/ipc2018-symple2.img

# SAT+AGL
./build-image.sh ../planners/ipc2018-saarplan/Apptainer ${OUTDIR}/ipc2018-saarplan.img || true
./build-image.sh ../planners/ipc2018-agl-cerberus/Apptainer ${OUTDIR}/ipc2018-agl-cerberus.img
./build-image.sh ../planners/ipc2018-agl-lapkt-dfs-plus/Apptainer ${OUTDIR}/ipc2018-lapkt-dfs-plus.img
./build-image.sh ../planners/ipc2018-agl-lapkt-dual-bfws/Apptainer ${OUTDIR}/ipc2018-lapkt-bfws.img # covers the three LAPKT BFWS planners
./build-image.sh ../planners/ipc2018-agl-mercury2014/Apptainer ${OUTDIR}/ipc2018-agl-mercury2014.img
./build-image.sh ../planners/ipc2018-agl-merwin/Apptainer ${OUTDIR}/ipc2018-agl-merwin.img
./build-image.sh ../planners/ipc2018-agl-fdss-2018/Apptainer ${OUTDIR}/ipc2018-fd-2018.img # covers both FDSS and FD-Remix

# OPT
./build-image.sh ../planners/ipc2014-opt-symba/Apptainer-01 ${OUTDIR}/ipc2014-opt-symba1.img
./build-image.sh ../planners/ipc2018-opt-complementary2/Apptainer ${OUTDIR}/ipc2018-opt-complementary2.img
./build-image.sh ../planners/ipc2018-opt-delfi/Apptainer ${OUTDIR}/ipc2018-opt-delfi.img
./build-image.sh ../planners/ipc2018-opt-metis/Apptainer ${OUTDIR}/ipc2018-opt-metis.img
./build-image.sh ../planners/ipc2018-opt-planning-pdbs/Apptainer ${OUTDIR}/ipc2018-opt-planning-pdbs.img
./build-image.sh ../planners/ipc2018-opt-scorpion/Apptainer ${OUTDIR}/ipc2018-opt-scorpion.img

# AGL
./build-image.sh ../planners/ipc2014-agl-jasper/Apptainer ${OUTDIR}/ipc2014-agl-jasper.img
./build-image.sh ../planners/ipc2014-agl-madagascar/Apptainer-MpC ${OUTDIR}/ipc2014-agl-mpc.img
./build-image.sh ../planners/ipc2014-agl-probe/Apptainer ${OUTDIR}/ipc2014-agl-probe.img
./build-image.sh ../planners/ipc2018-agl-olcff/Apptainer ${OUTDIR}/ipc2018-agl-olcff.img
./build-image.sh ../planners/ipc2018-agl-freelunch-madagascar/Apptainer ${OUTDIR}/ipc2018-agl-freelunch-madagascar.img

echo "Finished building images"
