#!/bin/bash


# DATAFILE=../../experiments/data/training-data-collect-eval/properties-hardest-sat.json.xz
# FTPATH=train_data/benchmark_features/ 
# TRAINFILE=train_data/training/epm_train_sat_journal.csv
# python read_trainexec.py $DATAFILE $FTPATH $TRAINFILE

DATAFILE=../../experiments/data/training-data-collect-eval/properties-hardest-opt.json.xz
FTPATH=train_data/benchmark_features/ 
TRAINFILE=train_data/training/epm_train_opt_journal.csv
python read_trainexec.py $DATAFILE $FTPATH $TRAINFILE


       \
      

