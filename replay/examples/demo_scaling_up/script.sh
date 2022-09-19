#!/bin/bash 

docker run --rm -ti \
  -v $(pwd)/out:/replay/out \
  -v $(pwd)/config.json:/replay/config.json \
  fredmoser/accessmod:latest Rscript -e replay/start.R
