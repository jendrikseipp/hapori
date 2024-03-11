#!/bin/sh
# $1 - folder name prefix

SUFFIX="sat"
FNAME="output_sat"
RUNTIMEDATA='runtimes_sat.csv'
SPLIT="split_sat.txt"
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
