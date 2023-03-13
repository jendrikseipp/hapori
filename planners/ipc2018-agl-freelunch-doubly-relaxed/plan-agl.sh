#!/bin/bash
#translate pddl to sas
set -e
p=/tmp/$$
rm -f output.sas
rm -f *.cnf
python3 /planner/translate/translate.py $1 $2
mv output.sas $p.sas

# bfs phase
java -Xmx5000m -jar /planner/fl-ipc18.jar bfs $p.sas $3 120

if [ ! -f $3 ]
then
# srt phase
java -Xmx5000m -jar /planner/fl-ipc18.jar srt $p.sas dummy $p.srt
/planner/incplan-lgl --outputSolverLike $p.srt > $p.sol
java -Xmx5000m -jar /planner/fl-ipc18.jar dec $p.sas $3 $p.sol
rm -f $p.srt $p.sas $p.sol
fi
