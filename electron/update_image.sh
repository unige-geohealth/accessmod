#!/bin/bash

# Exit on error
set -e

# Function to check required commands
check_command() {
    if ! command -v "$1" &> /dev/null
    then
        echo "Missing command $1"
        exit 1
    fi
}

# Check required commands
check_command 'docker'

# Variables
AM5_IMAGE="fredmoser/accessmod"
ARCHIVE_DIR="./resources/docker"
ARCHIVE_FILE="accessmod-docker.tar.gz"
ARCHIVE_PATH="${ARCHIVE_DIR}/${ARCHIVE_FILE}"
META_PATH="${ARCHIVE_DIR}/meta.json"
PACKAGE_CONF="./package.json"

# Ensure package.json exists
if [[ ! -e $PACKAGE_CONF ]]; then 
    echo "Missing $PACKAGE_CONF"
    exit 1
fi

# Prepare archive directory
mkdir -p $ARCHIVE_DIR

# Extract version from version.txt inside the latest image
docker pull $AM5_IMAGE:latest
AM5_VERSION=$(docker run --rm --entrypoint="" $AM5_IMAGE:latest cat version.txt)

# Check if version is empty
if [[ -z $AM5_VERSION ]]; then 
    echo "No version found"
    exit 1
fi

# Remove previous archive if exists
if [[ -e $ARCHIVE_PATH ]]; then 
    echo "Removing previous archive"
    rm $ARCHIVE_PATH 
fi

# Save the image with the specific version
docker save $AM5_IMAGE:latest > $ARCHIVE_PATH

# Create metadata 
echo '{"tag": "'$AM5_VERSION'","image_name": "'$AM5_IMAGE'","file": "'$ARCHIVE_FILE'"}' > $META_PATH

echo "Saved:"
cat $META_PATH | jq

# Notify user of success
echo "Docker image $AM5_IMAGE:$AM5_VERSION archived successfully."
