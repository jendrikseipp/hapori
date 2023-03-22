#!/bin/sh
# $1 - folder name prefix

SUFFIX="opt"
FNAME="output_opt"
RUNTIMEDATA='runtimes_opt.csv'
SPLIT="split_opt.txt"
TIMEOUT="1800"
IMAGES='images22'



LABEL_TYPE=discrete  # choices: binary discrete, normalized, time

python image_based_network.py \
  --input data --output $FNAME \
  --data $IMAGES --csv $RUNTIMEDATA \
  --split $SPLIT \
  --best val_loss \
  --label-type ${LABEL_TYPE} \
  --full-training \
  --random-validation 9 1 \
  --timelimit "${TIMEOUT}" \
