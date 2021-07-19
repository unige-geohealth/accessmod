#!/bin/bash
set -e
PUSH=""
DRY=""
AM_VERSION=$(cat ../version.txt)
AM_VERSION_LATEST="latest"
REPO="fredmoser"
GDAL_VERSION="3.1.3"
R_PACKAGES_DATE="2021-04-01"
ALPINE_VERSION="3.12.1"
R_VERSION="4.1.0"
GRASS_VERSION="7.8.4"

usage() { echo "Usage: $0 [-p push images] [-d dry]" 1>&2; exit 1; }

while getopts "hpd" opt; do
  case "$opt" in
    h|\?)
      usage
      ;;
    p)
      PUSH="true"
      ;;
    d)
      DRY="true"
      ;;
  esac
done
#shift $((OPTIND-1))


docker_build()
{
  build_usage()
  {
    echo "Usage: $0 [-n name] [-d Dockerfile dir ] [-c context ] [ -s save image dir ]" 1>&2; exit 1; 
  }

# Reset options index. 
unset OPTIND

# Check for flags
while getopts "hn:c:d:s:" opt; do
  case "$opt" in
    h|\?)
      build_usage
      ;;
    n)
      NAME=$OPTARG
      ;;
    d)
      FILE_DIR=$OPTARG
      ;;
    c)
      CONTEXT=$OPTARG
      ;;
    s)
      SAVE_IMAGE_DIR=$OPTARG
      ;;
  esac
done

  # set tag, names and path
  IMAGE=$REPO"/"$NAME
  TAG=$IMAGE":"$AM_VERSION
  TAG_LATEST=$IMAGE":"$AM_VERSION_LATEST
  IMG=$NAME".docker.gz"

  if [[ -n $DRY ]]
  then 
    echo "[dry] build + tag images $TAG_LATEST and $TAG" 
  else
    docker build \
      --progress plain \
      --build-arg GDAL_VERSION=$GDAL_VERSION \
      --build-arg R_VERSION=$R_VERSION \
      --build-arg R_PACKAGES_DATE=$R_PACKAGES_DATE \
      --build-arg GRASS_VERSION=$GRASS_VERSION \
      --build-arg ALPINE_VERSION=$ALPINE_VERSION \
      --file $FILE_DIR/Dockerfile \
      --tag $IMAGE \
      $CONTEXT

    echo "tag:" $TAG
    echo "image:" $IMAGE

    docker tag $IMAGE $TAG
    docker tag $IMAGE $TAG_LATEST
  fi

  #
  # Push to remote 
  #
  if [[ -n $PUSH ]]
  then 
    if [[ -n $DRY  ]]
    then
      echo "[dry] push docker image $TAG_LATEST and $TAG"
    else
      docker image push $TAG_LATEST
      docker image push $TAG
    fi
  fi

  #
  # Save image path 
  #
  if [[ -n $SAVE_IMAGE_DIR ]]; then
    if [[ -n $DRY  ]]
    then 
      echo "[dry] Export image + meta in $SAVE_IMAGE_DIR"
    else
      echo "Export image $TAG to $SAVE_IMAGE_DIR"
      echo '{"tag":"'$AM_VERSION'","image_name":"'$IMAGE'"}' > $SAVE_IMAGE_DIR"/meta.json"
      docker save $TAG | gzip > $SAVE_IMAGE_DIR/$IMG
    fi
  fi

}


docker_build \
  -n accessmod_grass \
  -d ./accessmod_grass \
  -c ./accessmod_grass

docker_build \
  -n accessmod_r \
  -d ./accessmod_r \
  -c ./accessmod_r

docker_build \
  -n accessmod \
  -d ./accessmod \
  -c ../. \
  -s ../electron/app/docker



