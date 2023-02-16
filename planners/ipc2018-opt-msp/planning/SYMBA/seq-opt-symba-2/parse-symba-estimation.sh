#! /bin/bash

cat $1 | grep -a "Running translator\\|^BOUND:\\|Step:" | sed -e "s/1. Running translator/(/" | sed -e "s/BOUND: /(bound /" | sed -e "s/ < [0-9]*,//" | sed -e "s/ total time://" | sed -e "s/\([0-9]*.[0-9]*\)s/\1)/" | sed -e "s/>> Step: [orignalSMAPDB]* [0-9, ]*\([fb]w\) f=\([0-9]*\), g=\([0-9]*\)/(step \1 \2 \3)/" | sed -e "s/SMAS [0-9]*,[0-9]*//" > $2
if [ -f $2 ]
  then if [ grep -q "(" $2 ]
	  echo ")" >> $2
       fi
fi	

