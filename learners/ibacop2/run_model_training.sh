#!/bin/bash

# Model should be created for running inside the ibacop-features.img
# So, for training the model follow these steps.
# 1.- Compile ibacop-features_dev.img and apptainer shell into it
# 2.- Run this script inside the image, then exit
# 3.- Build the ibacop-features.img
# 4.- Make sure the ibacop2 portfolio is pointing to image in (3)


TRAINFILE=train_data/training/epm_train_sat_journal.csv
MODELFILE=src/models/iba_model_rf_img_journal_sat.pkl 
python src/launcher/epm_training.py $TRAINFILE $MODELFILE

TRAINFILE=train_data/training/epm_train_opt_journal.csv
MODELFILE=src/models/iba_model_rf_img_journal_opt.pkl 
python src/launcher/epm_training.py $TRAINFILE $MODELFILE


