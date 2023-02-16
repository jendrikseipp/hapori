#! /bin/bash

# RF: commented this because T (solved) appears at the end and should appear at the beginning

#cat $2".tmp" | grep -a "Plan length: \\|Plan cost: \\|Generated [0-9]* state\\|Total time:\\|^(" | sed -e "s/step(s).//" | sed -e "s/state(s).//" | sed -e "s/Plan length: //" | sed -e "s/Plan cost: //" | sed -e "s/Generated //" | sed -e "s/state(s)./ /" | sed -e "s/Total time: //"  | sed -e "s/s//"
#> $2

# RF: one solution to make T appear at the beginning
cat $1 | grep -a "Solution found\.\\|no solution\\|^(" | sed -e "s/Solution found\./T/"| sed -e "s/Completely explored state space -- no solution!/NIL/" | sed -e "s/s//" > $1".1.tmp"

cat $1 |grep -a "Plan length: \\|Plan cost: \\|Generated [0-9]* state\\|Total time:\\|^(" | sed -e "s/step(s).//" | sed -e "s/state(s).//" | sed -e "s/Plan length: //" | sed -e "s/Plan cost: //" | sed -e "s/Generated //" | sed -e "s/state(s)./ /" | sed -e "s/Total time: //"  | sed -e "s/s//" > $1".2.tmp"

cat $1".1.tmp" $1".2.tmp" > $2

rm $1".1.tmp" $1".2.tmp"


