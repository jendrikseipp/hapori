#! /bin/bash

set -euo pipefail

cd $(dirname "$0")

OUTDIR=../images/

# OPT
./build-image.sh ../planners/ipc2014-opt-symba/Singularity-01 ${OUTDIR}/ipc2014-opt-symba1.img
./build-image.sh ../planners/ipc2018-opt-complementary2/Singularity ${OUTDIR}/ipc2018-opt-complementary2.img
./build-image.sh ../planners/ipc2018-opt-decstar/Singularity ${OUTDIR}/ipc2018-opt-decstar.img
./build-image.sh ../planners/ipc2018-opt-scorpion/Singularity ${OUTDIR}/ipc2018-opt-scorpion.img
./build-image.sh ../planners/ipc2018-opt-scorpion-nodiv/Singularity ${OUTDIR}/ipc2018-opt-scorpion-nodiv.img
#./build-image.sh ../planners/ipc2018-opt-delfi1/Singularity ${OUTDIR}/ipc2018-opt-delfi.img
./build-image.sh ../planners/ipc2018-opt-delfi1/Singularity-blind ${OUTDIR}/ipc2018-opt-delfi-blind.img
./build-image.sh ../planners/ipc2018-opt-delfi1/Singularity-celmcut ${OUTDIR}/ipc2018-opt-delfi-celmcut.img
./build-image.sh ../planners/ipc2018-opt-delfi1/Singularity-ipdb ${OUTDIR}/ipc2018-opt-delfi-ipdb.img
./build-image.sh ../planners/ipc2018-opt-delfi1/Singularity-ipdb-60s ${OUTDIR}/ipc2018-opt-delfi-ipdb-60s.img
./build-image.sh ../planners/ipc2018-opt-delfi1/Singularity-mas-sccdfp ${OUTDIR}/ipc2018-opt-delfi-mas-sccdfp.img
./build-image.sh ../planners/ipc2018-opt-delfi1/Singularity-mas-sccdfp-60s ${OUTDIR}/ipc2018-opt-delfi-mas-sccdfp-60s.img
./build-image.sh ../planners/ipc2018-opt-delfi1/Singularity-mas-miasm ${OUTDIR}/ipc2018-opt-delfi-mas-miasm.img
./build-image.sh ../planners/ipc2018-opt-planning-pdbs/Singularity ${OUTDIR}/ipc2018-opt-planning-pdbs.img

# AGL
./build-image.sh ../planners/ipc2014-agl-madagascar/Singularity-MpC ${OUTDIR}/ipc2014-agl-mpc.img
./build-image.sh ../planners/ipc2014-agl-mercury/Singularity ${OUTDIR}/ipc2014-agl-mercury.img
./build-image.sh ../planners/ipc2014-agl-jasper/Singularity ${OUTDIR}/ipc2014-agl-jasper.img
./build-image.sh ../planners/ipc2014-agl-probe/Singularity ${OUTDIR}/ipc2014-agl-probe.img
./build-image.sh ../planners/ipc2018-agl-decstar/Singularity ${OUTDIR}/ipc2018-agl-decstar.img
./build-image.sh ../planners/ipc2018-agl-fd-remix/Singularity ${OUTDIR}/ipc2018-agl-fd-remix.img
./build-image.sh ../planners/ipc2018-agl-lapkt-dual-bfws/Singularity ${OUTDIR}/ipc2018-agl-lapkt-dual-bfws.img
./build-image.sh ../planners/ipc2018-agl-lapkt-bfws-pref/Singularity ${OUTDIR}/ipc2018-agl-lapkt-bfws-pref.img
./build-image.sh ../planners/ipc2018-agl-lapkt-poly-bfws/Singularity ${OUTDIR}/ipc2018-agl-lapkt-poly-bfws.img
./build-image.sh ../planners/ipc2018-agl-olcff/Singularity ${OUTDIR}/ipc2018-agl-olcff.img
./build-image.sh ../planners/ipc2018-agl-saarplan/Singularity ${OUTDIR}/ipc2018-agl-saarplan.img
./build-image.sh ../planners/ipc2018-agl-cerberus/Singularity ${OUTDIR}/ipc2018-agl-cerberus.img
./build-image.sh ../planners/ipc2018-agl-merwin/Singularity.agl ${OUTDIR}/ipc2018-agl-merwin.img

# SAT
./build-image.sh ../planners/ipc2018-agl-merwin/Singularity.sat ${OUTDIR}/ipc2018-sat-merwin.img 
./build-image.sh ../planners/ipc2018-sat-fdss-2018/Singularity ${OUTDIR}/ipc2018-sat-fdss-2018.img

echo "Finished building images"
