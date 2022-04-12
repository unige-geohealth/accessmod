#!/bin/bash 

docker run --rm -ti \
  -v $(pwd)/out:/replay/out \
  -v $(pwd)/config.json:/replay/config.json \
  fredmoser/accessmod:5.7.20 Rscript -e replay.R
