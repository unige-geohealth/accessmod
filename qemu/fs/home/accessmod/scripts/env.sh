#!/bin/bash

#
# Variables (re)loaded within scripts
#

# Docker Hub API URLs
export AM5_DOCKER_HUB="${AM5_HUB_API}/repositories/${AM5_REPO}"
export AM5_DOCKER_API_URL="${AM5_DOCKER_HUB}/tags/?page_size=100&page=1&name=5"

# Save original version from environment
export AM5_VERSION_ORIG="${AM5_VERSION}"

# Note: AM5_VERSION will be (re)set using _get_version after sourcing helpers.sh in start.sh

# UI and system variables
export HEALTH_URL="http://localhost:${AM5_PORT_APP}/"
export WIDTH=80
export HEIGHT=30
export BACKTITLE="AccessMod 5"
export TITLE="Version Manager"

# Temporary file paths
export VERSIONS_RAW=""
export VERSIONS_REMOTE=""
export VERSIONS_CACHE_FILE="/tmp/versions.json"
export TMP_FILE="/tmp/res"

# Enable interactive mode by default
if [[ -z "${INTERACTIVE}" ]]; then
    INTERACTIVE=1
fi

# GitHub repository URL for script updates
export AM5_GITHUB_REPO="https://github.com/unige-geohealth/accessmod"
