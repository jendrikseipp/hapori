#! /bin/bash -l
### Set name.
#SBATCH --job-name=2023-03-22-train-delfi-sat
### Redirect stdout and stderr.
#SBATCH --output=slurm_sat.log
#SBATCH --error=slurm_sat.err
### Let later steps append their logs to the output and error files.
#SBATCH --open-mode=append
### Set partition.
#SBATCH --partition=infai_3
### Set quality-of-service group.
#SBATCH --qos=normal
### Set wall-clock time limit per task.
#SBATCH --time=0
### Set memory limit.
#SBATCH --mem-per-cpu=2000M
### Set number of cores per task.
#SBATCH --cpus-per-task=40
### Number of tasks in array job.
#SBATCH --array=1-1
### Adjustment to priority ([-2147483645, 2147483645]).
#SBATCH --nice=0
### Send mail? Mail type can be e.g. NONE, END, FAIL, ARRAY_TASKS.
#SBATCH --mail-type=END
#SBATCH --mail-user=patrick.ferber@unibas.ch
### Extra options.
## (not used)



# Set a soft memory limit to guard against the cgroup mechanism failing (see SlurmEnvironment docs).
ulimit -Sv 35123200
module purge
module load Python/2.7.11-goolf-1.7.20
module load CMake/3.4.3-goolf-1.7.20
source venv/bin/activate
./train_sat.sh