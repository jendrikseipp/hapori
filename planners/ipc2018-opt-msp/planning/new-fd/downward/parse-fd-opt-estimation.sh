#! /bin/bash

echo "(" > $2
cat $1 | grep -a "^f = " | sed -e "s/f = \([0-9]*\) \[[ ,.a-z0-9=]*\]/(f \1)/" >> $2
echo ")" >> $2

