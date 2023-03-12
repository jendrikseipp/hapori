#!/bin/bash
set -e
p=/tmp/$$
/planner/MpC $1 $2 -L -b $p.output
/planner/incplan-lgl --outputSolverLike $p.output.cnf > $p.incplanOutput.txt
grep -E "^[0-9\-]" $p.incplanOutput.txt > $p.solution.txt
plen=`grep "Final Makespan" $p.incplanOutput.txt | cut -d: -f 3`
#echo $1 $2 $p $plen "lol"
/planner/MpC $1 $2 -s $p.solution.txt -T $plen -Q -o $3
rm $p.*
