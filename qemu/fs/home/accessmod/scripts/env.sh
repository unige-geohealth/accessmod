#!/bin/bash

# 
# Variables (re)loaded within scripts
#
AM5_DOCKER_HUB="${AM5_HUB_API}/repositories/$AM5_REPO"
AM5_DOCKER_API_URL="$AM5_DOCKER_HUB/tags/?page_size=100&page=1&name=$AM5_MIN_VERSION"

# Save original version from environment
AM5_VERSION_ORIG="$AM5_VERSION"

# Initialize AM5_VERSION using the getter function
# (This will be called after helpers.sh is sourced in start.sh)

HEALTH_URL="http://localhost:$AM5_PORT_APP/"
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
