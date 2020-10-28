#!/bin/bash
set -e
AM5_VERSION=$(cat ../version.txt)
REPO="fredmoser"
param=$1

docker_build()
{
  TARGET=$REPO"/"$1
  docker build $param -t $TARGET $1
  docker tag $TARGET $TARGET":"$AM5_VERSION
}


docker_build am5_common
docker_build am5_grass
docker_build am5_r
docker_build am5_app

