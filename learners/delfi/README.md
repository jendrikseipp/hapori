# Training:
- change to /learners/delfi/train/
- use python2.7
- install packages from /learners/delfi/train/requirements.txt
- update the files in /learners/delfi/train/data/ if desired
  - `runtimes_opt.csv` is a csv files which contains for each task the required 
    time of each planner or `-` if the planner was unable to solve the task.
  - `split_opt.txt` informs the process which tasks are used for training, validating
     and testing. Just add all tasks to train and leave the other sets empty.
- run `train_*.sh` to start the training process locally or `slurm_*.sh` if you
  want to launch the training on a Slurm grid environment.


