#!/bin/sh

# source environment_vars
. /etc/profile

if [ -z "$AM5_PORT_SHINY" ]; then
 AM5_PORT_SHINY=3434
fi

if [ -z "$AM5_PORT_DOCKER" ]; then
 AM5_PORT_DOCKER=8080
fi

docker run \
  -p $AM5_PORT_DOCKER:$AM5_PORT_SHINY \
  -v am_data:/data/dbgrass \
  -v am_tmp:/tmp \
  -d \
  $AM5_IMAGE \
  Rscript \
  --vanilla \
  run.r \
  $AM5_PORT_SHINY
