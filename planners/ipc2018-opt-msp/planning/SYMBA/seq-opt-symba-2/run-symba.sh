#! /bin/bash
# Usage: ./run-symba SOURCE-PATH DOMAIN-FILE-PATH PROBLEM-FILE-PATH PLAN-SOL-FILE-PATH LOG-FILE-PATH
# cd /Users/dborrajo/planning/symba/
#export PATH=/opt/local/bin:/opt/local/sbin:/bin:/usr/bin:/etc:/usr/sbin:/sbin:./:/usr/local/bin
\rm downward.tmp.* output output.sas plan_numbers_and_cost elapsed.time lama-output* $4 $5
echo "run-symba.sh script start"
#./plan $1 $2 $3 > $4
echo "$(dirname "$0")/plan $2 $3 $4 > $5"
#echo "$1SYMBA/seq-opt-symba-2/plan-blind $2 $3 $4 > $5"
ulimit -Sv 8000000; "$(dirname "$0")"/plan $2 $3 $4 > $5
#ulimit -Sv 8000000; $1SYMBA/seq-opt-symba-2/plan-blind $2 $3 $4 > $5
echo "run-symba.sh script end"
# ./plan ../domains/ipc2011/seq-opt/elevators/domain/domain.pddl ../domains/ipc2011/seq-opt/elevators/problems/p08.pddl plan.sol > /media/sf_Compartida_Ubuntu_VirtualBox/log-symba
