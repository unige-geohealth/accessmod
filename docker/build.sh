#!/bin/bash
set -e
PUSH=""
AM_VERSION=$(cat ../version.txt)
AM_VERSION_LATEST="latest"
REPO="fredmoser"
GDAL_VERSION="3.1.3"
R_PACKAGES_DATE="2020-12-20"
ALPINE_VERSION="3.12.1"
R_VERSION="4.0.3"
GRASS_VERSION="7.8.4"
ELECTRON_DOCKER_PATH="../electron/app/docker" 


while getopts "h?p" opt; do
    case "$opt" in
    h|\?)
      echo -e 'Usage: \n build.sh [-p (push to docker)]'
        exit 0
        ;;
    p)  PUSH="true"
        ;;
    esac
done


docker_build()
{
  NAME=$1
  CONTEXT=$2
  COPY_ELECTRON=""
  IMAGE=$REPO"/"$NAME
  TAG=$IMAGE":"$AM_VERSION
  TAG_LATEST=$IMAGE":"$AM_VERSION_LATEST
  IMG=$NAME".docker.gz"

  while getopts "h?e" opt; do
    case "$opt" in
      h|\?)
        echo -e 'Usage: docker_build [-e (push image to electron)] <folder/image name> <context>'
        exit 0
        ;;
      e) COPY_ELECTRON="true"
        ;;
    esac
  done

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
  docker tag $IMAGE $TAG_LATEST
  if [ -n $PUSH ]
  then 
    docker image push $TAG_LATEST
    docker image push $TAG
  fi

  #
  # Update electron base image
  #
  if [[ -n $COPY_ELECTRON ]]; then
    echo $COPY_ELECTRON
    echo "Export image $TAG to $ELECTRON_DOCKER_PATH"
    echo '{"tag":"'$AM_VERSION'","image_name":"'$IMAGE'"}' > $ELECTRON_DOCKER_PATH"/meta.json"
    docker save $TAG | gzip > $ELECTRON_DOCKER_PATH/$IMG 
  fi

}

#
# Docker build each image
# NOTE : context for 'accessmod' is the main dir (./..)
#
#  CMD NAME CONTEXT ( COPY_ELECTRON )
docker_build accessmod_grass accessmod_grass
docker_build accessmod_r accessmod_r
docker_build accessmod ../. -e


