#!/bin/bash

#
# Variables (re)loaded within scripts
#

# Docker Hub API URLs
AM5_DOCKER_HUB="${AM5_HUB_API}/repositories/${AM5_REPO}"
AM5_DOCKER_API_URL="${AM5_DOCKER_HUB}/tags/?page_size=100&page=1&name=5"

# Save original version from environment
AM5_VERSION_ORIG="${AM5_VERSION}"

# Note: AM5_VERSION will be (re)set using _get_version after sourcing helpers.sh in start.sh

# UI and system variables
HEALTH_URL="http://localhost:${AM5_PORT_APP}/"
WIDTH=80
HEIGHT=30
BACKTITLE="AccessMod 5"
TITLE="Version Manager"

# Temporary file paths
VERSIONS_RAW=""
VERSIONS_REMOTE=""
VERSIONS_CACHE_FILE="/tmp/versions.json"
TMP_FILE="/tmp/res"

# Enable interactive mode by default
if [[ -z "${INTERACTIVE}" ]]; then
    INTERACTIVE=1
fi

# GitHub repository URL for script updates
AM5_GITHUB_REPO="https://github.com/unige-geohealth/accessmod"
