#!/bin/sh
# $1 - folder name prefix

SUFFIX="sat"
FNAME="output_sat"
RUNTIMEDATA='full-sat-quality.csv'
RUNTIMEDATA='hardest-sat-quality.csv'
SPLIT="full-split_sat_notest.txt"
SPLIT="full-split_sat.txt"
SPLIT="hardest-split_sat_notest.txt"
SPLIT="hardest-split_sat.txt"
TIMEOUT="1800"
IMAGES='images23'



LABEL_TYPE=sat  # choices: binary discrete, normalized, time

python image_based_network.py \
  --input data --output $FNAME \
  --data $IMAGES --csv $RUNTIMEDATA \
  --split $SPLIT \
  --best val_loss \
  --label-type ${LABEL_TYPE} \
  --full-training \
  --random-validation 9 1 \
  --timelimit "${TIMEOUT}" \
