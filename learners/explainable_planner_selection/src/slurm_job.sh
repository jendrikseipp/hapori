#!/bin/bash

#SBATCH --job-name=simple_delfi
#SBATCH --time=6:00:00
#SBATCH --mem=3G
#SBATCH --cpus-per-task=1
#SBATCH -o simple_delfi%j.out # Standard output
#SBATCH -e simple_delfi%j.err # Standard error
#SBATCH --partition infai_2

## If an array job is given, then it is expected that N regular
## expressions are the first N parameters and the SLURM_ARRAY_TASK_ID
## determines which of those expression describes the test and which the
## validation problems.

ulimit -Sv 3072000

##LOAD MODULES
module load Python/3.6.6-foss-2018b
source ~/bin/kerascpu3/bin/activate


##SETUP PROBLEM TO RUN IF ARRAY JOB
if [ ! -z ${SLURM_ARRAY_TASK_ID+x} ]; then
	if [ -z ${SLURM_ARRAY_TASK_COUNT+x} ]; then
	    (>&2 echo "error: Error with slurm array variables");
	    exit 4;
	fi
    echo "Slurm array id: $SLURM_ARRAY_TASK_ID/$SLURM_ARRAY_TASK_COUNT"
    CONFIG=$(cat $1 | tail "-n+${SLURM_ARRAY_TASK_ID}" | head -n1)
fi

echo $CONFIG
./train_ml.py $CONFIG

deactivate
