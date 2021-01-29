#!/bin/bash
set -e
AM_VERSION=$(cat ../version.txt)
REPO="fredmoser"
IMAGEDIR="images"
GDAL_VERSION="3.1.3"
R_PACKAGES_DATE="2020-12-20"
ALPINE_VERSION="3.12.1"
R_VERSION="4.0.3"
GRASS_VERSION="7.8.4"

# `docker_build <folder/image name> <context>`
docker_build()
{
  NAME=$1
  CONTEXT=$2
  IMAGE=$REPO"/"$NAME
  TAG=$IMAGE":"$AM_VERSION
  IMG=$NAME".docker.gz"
  docker build \
    --progress plain \
    --build-arg GDAL_VERSION=$GDAL_VERSION \
    --build-arg R_VERSION=$R_VERSION \
    --build-arg PACKAGES_DATE=$PACKAGES_DATE \
    --build-arg GRASS_VERSION=$GRASS_VERSION \
    --build-arg ALPINE_VERSION=$ALPINE_VERSION \
    --file $NAME/Dockerfile \
    --tag $IMAGE \
    $CONTEXT
  echo "tag:" $TAG
  echo "image:" $IMAGE
  docker tag $IMAGE $TAG
  #
  # Extract an image
  #
  echo "Export image $TAG"
  docker save $TAG | gzip > $IMAGEDIR/$IMG

}

#
# Docker build each image
# NOTE : context for 'accessmod' is the main dir (./..)
#
docker_build accessmod_grass accessmod_grass
docker_build accessmod_r accessmod_r
docker_build accessmod ../.


