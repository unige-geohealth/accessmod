#!/bin/bash

START_TITLE="Starting $AM5_VERSION..."

. $AM5_SCRIPTS_FOLDER/env.sh
. $AM5_SCRIPTS_FOLDER/message.sh 
. $AM5_SCRIPTS_FOLDER/helpers.sh 

# message
_msg "Start requested..." --duration 2 --title "$START_TITLE"

if [[ ! -e $AM5_SCRIPTS_FOLDER/ready ]]
then
  _msg "Not ready :(" --duration 2 --title "$START_TITLE"
  exit 0
fi

#
# Load image if needed
#
if [[ -z "`docker images -q $AM5_REPO`" ]]
then
  _msg "Mising at least an image, pull $AM5_VERSION" --title "$START_TITLE"
  docker pull $AM5_REPO:$AM5_VERSION
fi

RUNNING=`docker ps -qa --filter name=$AM5_NAME`

if [[ -n "$RUNNING" ]]
then
  _msg "Container $AM5_NAME is running, stop and remove" --title "$START_TITLE"
  docker stop $RUNNING
  docker rm $RUNNING
fi

#
# Start docker service
#
_msg "Container $AM5_NAME is starting, please wait" --title "$START_TITLE"

docker run \
  --name $AM5_NAME \
  --health-cmd="wget --spider $HEALTH_URL" \
  --health-interval=1m \
  --health-retries=10 \
  --health-start-period=10s \
  -p $AM5_PORT_APP:$AM5_PORT_APP \
  -p $AM5_PORT_HTTP:$AM5_PORT_HTTP \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v /tmp:/tmp \
  -v am_data_logs:/data/logs \
  -v am_data_cache:/data/cache \
  -v am_data_grass:/data/dbgrass \
  -d \
  --restart unless-stopped \
  $AM5_REPO:$AM5_VERSION \
  Rscript \
  --vanilla \
  run.r \
  $AM5_PORT_APP \
  $AM5_PORT_HTTP \
  $AM5_PORT_HTTP_PUBLIC

_msg "Container $AM5_NAME is starting ( this could take a minute )" \
  --duration 10 \
  --title "$START_TITLE"

