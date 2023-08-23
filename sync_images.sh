#!/bin/bash

# Source and destination registries
SRC_REGISTRY="docker.io"
DST_REGISTRY="git.unepgrid.ch"

# Image repository name
SRC_REPO="fredmoser"
DST_REPO="geohealth"

# List of architectures to sync
ARCHITECTURES=("arm64" "amd64")
IMAGES=("accessmod" "accessmod_base")

# Function to check if user is logged in
check_login() {
    local repo=$1
    skopeo login --get-login "$repo" || {
        cat <<- EOM
Failed to authenticate.
Use skopeo login $repo
EOM
        exit 1
    }
}

# Check login for each repository
for REPO in $SRC_REGISTRY $DST_REGISTRY; do
    check_login $REPO
done

# For each image...
for IMG_NAME in "${IMAGES[@]}"; do
  # Get all the tags for the current image
  TAGS=$(skopeo list-tags docker://${SRC_REGISTRY}/${SRC_REPO}/${IMG_NAME} | jq -r '.Tags[]')
  # Loop through each tag and sync it
  for TAG in $TAGS; do
    echo "COPYING ${TAG} for ${IMG_NAME}"
    skopeo copy \
      --multi-arch all \
      docker://${SRC_REGISTRY}/${SRC_REPO}/${IMG_NAME}:${TAG} \
      docker://${DST_REGISTRY}/${DST_REPO}/${IMG_NAME}:${TAG}
    done
done


