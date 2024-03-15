# Models 

The following models were trained on the entire available data
## Optimal 

 * [main-binary] train/delfi2024_models/full/binary/output_opt_10_03_2024
 * [main-discrete] train/delfi2024_models/full/discrete/output_opt_09_03_2024

## Satisficing

 * [main-sat] train/delfi2024_models/full/sat/output_sat_13_03_2024


## Additional models (sanity check)
The following models were trained on the entire available data minus a held out test set
 * [binary] train/delfi2024_models/full/binary/output_opt_notest_10_03_2024
 * [discrete] train/delfi2024_models/full/discrete/output_opt_notest_09_03_2024
 * [sat] train/delfi2024_models/full/sat/output_sat_notest_13_03_2024

## Additional models - restricted training set
The following models were trained on the balanced set of instances across domains
 * [binary] train/delfi2024_models/harderst/binary/output_opt_10_03_2024
 * [discrete] train/delfi2024_models/harderst/discrete/output_opt_11_03_2024
 * [sat] train/delfi2024_models/harderst/sat/output_sat_14_03_2023

## Additional models - restricted training set (sanity check)
The following models were trained on the balanced set of instances across domains minus a held out test set (same as above)
 * [binary] train/delfi2024_models/harderst/binary/output_opt_notest_10_03_2024
 * [discrete] train/delfi2024_models/harderst/discrete/output_opt_notest_11_03_2024
 * [sat] train/delfi2024_models/harderst/sat/output_sat_notest_14_03_2023






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


