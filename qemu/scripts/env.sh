#!/bin/bash

# 
# Variables (re)loaded within scripts
#
AM5_DOCKER_HUB="${AM5_HUB_API}/repositories/$AM5_REPO"
AM5_DOCKER_API_URL="$AM5_DOCKER_HUB/tags/?page_size=100&page=1&name=$AM5_MIN_VERSION"

AM5_VERSION_ORIG=$AM5_VERSION

# stored version. Set during provision and _update function
AM5_VERSION=$(cat $AM5_VERSION_FILE)

AM5_VERSION_ORIG=$AM5_VERSION


HEALTH_URL="http://localhost:$AM5_PORT_HTTP/status"
WIDTH=80
HEIGHT=30
BACKTITLE="AccessMod 5"
TITLE="Version Manager"
VERSIONS_RAW="";
VERSIONS_REMOTE=""
VERSIONS_CACHE_FILE="/tmp/versions.json"
TMP_FILE="/tmp/res"


if [ -z "$INTERACTIVE" ]; then
  INTERACTIVE=1
fi


if [ -z "$AM5_VERSION" ]
then
  $AM5_VERSION=$AM5_VERSION_ORIG
fi




