#!/bin/bash 

check_command()
{
  if [ -z `command -v $1` ]; 
  then 
    echo "Missing command $1";
    exit 1;
  fi
}
check_command 'docker'

AM5_IMAGE="fredmoser/accessmod"
AM5_VERSION=$(docker run --rm $AM5_IMAGE:latest cat version.txt)
ARCHIVE_DIR=./app/docker
ARCHIVE_PATH=$ARCHIVE_DIR/accessmod-docker.tar.gz
META_PATH=$ARCHIVE_DIR/meta.json
PACKAGE_CONF=./package.json

if [[ -z $AM5_VERSION ]]
then 
  echo "no version"
  exit 1
fi

if [[ ! -e $PACKAGE_CONF ]]
then 
  echo "no $PACKAGE_CONF"
  exit 1
fi

if [[ -e $ARCHIVE_PATH ]]
then 
  echo "rm previous archive"
  rm $ARCHIVE_PATH 
fi

mkdir -p $ARCHIVE_DIR

docker pull $AM5_IMAGE:$AM5_VERSION 
docker save $AM5_IMAGE:$AM5_VERSION > $ARCHIVE_PATH

echo "{\"tag\":\"$AM5_VERSION\",\"image_name\":\"$AM5_IMAGE\"}" > $META_PATH

