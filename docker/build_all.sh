#!/bin/bash
set -e
AM5_VERSION=$(cat ../version.txt)
REPO="fredmoser"

# `docker_build <folder/image name> <context>`
docker_build()
{
  NAME=$1
  CONTEXT=$2
  IMAGE=$REPO"/"$NAME
  TAG=$IMAGE":"$AM5_VERSION
  docker build \
    --file $NAME/Dockerfile \
    --tag $IMAGE \
    $CONTEXT
  echo "tag:" $TAG
  echo "image:" $IMAGE
  docker tag $IMAGE $TAG
}

#
# Docker build each image 
#
docker_build am5_common .
docker_build am5_grass .
docker_build am5_r .
docker_build am5_app ..

