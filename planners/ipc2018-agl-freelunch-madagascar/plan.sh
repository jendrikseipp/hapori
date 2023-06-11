#!/bin/bash
# parameters: 1 - domain file, 2 - problem file, 3 plan file, 4 - path to MpC executable, 5 - path to incplan-lgl executable
set -e
p=/tmp/$$
$4 $1 $2 -L -b $p.output
$5 --outputSolverLike $p.output.cnf > $p.incplanOutput.txt
grep -E "^[0-9\-]" $p.incplanOutput.txt > $p.solution.txt
plen=`grep "Final Makespan" $p.incplanOutput.txt | cut -d: -f 3`
#echo $1 $2 $p $plen "lol"
$4 $1 $2 -s $p.solution.txt -T $plen -Q -o $3
rm $p.*
