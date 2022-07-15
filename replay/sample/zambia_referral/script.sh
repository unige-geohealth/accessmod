#!/bin/bash 
DB="/Users/fxi/Documents/Travail/oms/accessmod/source/AccessMod_server.git/data"

docker run --rm -ti \
  -v $DB:/data/dbgrass \
  -v $(pwd)/out:/replay/out \
  -v $(pwd)/config.json:/replay/config.json \
  fredmoser/accessmod:5.7.20-alpha-1.0 Rscript replay.R
