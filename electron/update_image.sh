#!/bin/bash

# Exit on error
set -e

# Variables
ARCHIVE_DIR="./resources/docker"
ARCHIVE_FILE="accessmod-docker.tar.gz"
ARCHIVE_PATH="${ARCHIVE_DIR}/${ARCHIVE_FILE}"
META_PATH="${ARCHIVE_DIR}/meta.json"
PACKAGE_CONF="./package.json"
VERSION_FILE="${ARCHIVE_DIR}/version"
DOWNLOADED_IMAGE="${ARCHIVE_DIR}/docker_image.tar.gz"
AM5_IMAGE="fredmoser/accessmod"

# Ensure package.json exists
if [[ ! -e $PACKAGE_CONF ]]; then 
    echo "Missing $PACKAGE_CONF"
    exit 1
fi

# Ensure archive directory exists
mkdir -p $ARCHIVE_DIR

# Check if the downloaded artifact files exist
if [[ ! -e $VERSION_FILE ]]; then
    echo "Missing version file at $VERSION_FILE"
    exit 1
fi

if [[ ! -e $DOWNLOADED_IMAGE ]]; then
    echo "Missing Docker image archive at $DOWNLOADED_IMAGE"
    exit 1
fi

# Read version from the version file
AM5_VERSION=$(cat $VERSION_FILE)

# Check if version is empty
if [[ -z $AM5_VERSION ]]; then 
    echo "No version found in $VERSION_FILE"
    exit 1
fi

# If the downloaded image is not already at the expected location, move/rename it
if [[ "$DOWNLOADED_IMAGE" != "$ARCHIVE_PATH" ]]; then
    # Remove previous archive if exists
    if [[ -e $ARCHIVE_PATH ]]; then 
        echo "Removing previous archive"
        rm $ARCHIVE_PATH 
    fi
    
    echo "Moving Docker image archive to $ARCHIVE_PATH"
    cp $DOWNLOADED_IMAGE $ARCHIVE_PATH
fi

# Create metadata using jq
jq -n \
  --arg tag "$AM5_VERSION" \
  --arg image_name "$AM5_IMAGE" \
  --arg file "$ARCHIVE_FILE" \
  '{tag: $tag, image_name: $image_name, file: $file}' > $META_PATH

echo "Saved:"
cat $META_PATH | jq

# Notify user of success
echo "Docker image $AM5_IMAGE:$AM5_VERSION metadata created successfully."
