#!/bin/sh

# source environment_vars
. /etc/profile

if [ -z "$AM5_PORT_APP" ]; then
 AM5_PORT_APP=3434
fi

if [ -z "$AM5_PORT_HTTP" ]; then
 AM5_PORT_HTTP=5099
fi

docker run \
  -p $AM5_PORT_APP:$AM5_PORT_APP \
  -v am_data:/data/dbgrass \
  -v am_tmp:/tmp \
  -d \
  --restart unless-stopped \
  $AM5_IMAGE \
  Rscript \
  --vanilla \
  app.r \
  $AM5_PORT_APP \
  $AM5_PORT_HTTP

docker run \
  -p $AM5_PORT_HTTP:$AM5_PORT_HTTP \
  -v am_tmp:/tmp \
  -d \
  --restart unless-stopped \
  $AM5_IMAGE \
  Rscript \
  --vanilla \
  http.r \
  $AM5_PORT_HTTP
