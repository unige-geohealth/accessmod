#!/bin/bash
set -e

# source environment_vars
. /etc/profile

if [[ ! -e $AM5_SCRIPTS_FOLDER/ready ]]
then
  exit 0
fi

AM5_VERSION_ORIG=$AM5_VERSION
AM5_NAME="accessmod"
AM5_VERSION=`cat $AM5_VERSION_FILE`
AM5_IMAGE="$AM5_REPO:$AM5_VERSION"
WIDTH=60
HEIGHT=30
BACKTITLE="AccessMod"
TITLE="Starting $AM5_NAME ..."
INTERACTIVE=1
#`if [ $0 = "start.sh" ];then echo "1";else echo "";fi`

_msg(){
  s=$2
  if [[ -z "$s" ]]
  then 
    s="2"
  fi

  if [[ -n $INTERACTIVE ]]
  then
    dialog \
      --backtitle "$BACKTITLE" \
      --title "$TITLE" \
      --infobox "$1" 3 70
          sleep $s 
        else
          echo $1
  fi
}

#
# Load image if needed
#
if [[ -z "`docker images -q $AM5_REPO`" ]]
then
  if [[ -e $AM5_ARCHIVE_DOCKER && $AM5_VERSION = $AM5_VERSION_ORIG ]]
  then
    _msg "Version $AM5_VERSION not installed: import from archive"
    docker load < $AM5_ARCHIVE_DOCKER
  else
    _msg "Version $AM5_VERSION not installed: remote pull"
    docker pull $AM5_IMAGE
  fi
fi

RUNNING=`docker ps -qa --filter name=$AM5_NAME`

if [[ -n "$RUNNING" ]]
then
  _msg "Container $AM5_NAME is running, stop and remove"
  docker stop $RUNNING
  docker rm $RUNNING
fi

#
# Start docker service
#
_msg "Container $AM5_NAME is starting, please wait"

docker run \
  --name $AM5_NAME \
  --health-cmd='wget --spider http://localhost:$AM5_PORT_HTTP/status' \
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
  $AM5_IMAGE \
  Rscript \
  --vanilla \
  run.r \
  $AM5_PORT_APP \
  $AM5_PORT_HTTP \
  $AM5_PORT_HTTP_PUBLIC

_msg "Container $AM5_NAME is starting ( this could take a minute )" 10
