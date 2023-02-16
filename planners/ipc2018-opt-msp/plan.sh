#!/bin/bash

domain=$1
problem=$2
plan_file=$3

domain_dir=`dirname $1`
domain_name=`basename $domain_dir`
problem_file=`basename $2`

WORK_DIR=`mktemp -d -p "./"`
echo "Temporal MSP execution dir: " $WORK_DIR
chmod +rwx $WORK_DIR

# deletes the temp directory
# function cleanup {
#   rm -rf "$WORK_DIR"
#   echo "Deleted temp working directory $WORK_DIR"
# }

# register the cleanup function to be called on the EXIT signal
# REMOVE COMMENT IN THE NEXT LINE TO REMOVE THE TEMPORAL DIRECTORY
#trap cleanup EXIT


cp $1 $WORK_DIR/domain.pddl
cp $2 $WORK_DIR

"$(dirname "$0")"/metasearch-exe --eval '(sayrun (meta-search-from-exe))' $domain_name $problem_file "$WORK_DIR/" "$WORK_DIR/" 1800 $plan_file "$(dirname "$0")/"


