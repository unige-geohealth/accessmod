#!/bin/sh
set -e

# source environment_vars
. /etc/profile

#
# Accessmod part
#
apk add docker
rc-update add docker boot
rc-update add local default
service docker start
sleep 10
if service -e docker ; then 
  docker pull $AM5_IMAGE
  docker volume create am_data
  docker volume create am_tmp
  mv /tmp/start.sh /etc/local.d/accessmod.start
  mv /tmp/info.sh /etc/local.d/issue_update_info.start
  chmod +x /etc/local.d/accessmod.start
  chmod +x /etc/local.d/issue_update_info.start
  # Upate content of /etc/issue (login message)
  cp /etc/local.d/issue_update_info.start /etc/periodic/15min/issue_update_info.sh
else 
  echo "Docker not started, provision failed"
  exit 1
fi

