#!/bin/bash 
#------------------------------------------------------------------------------#
#
#  Build AccessMod Base Image 
#  (c) unige.ch 
#  
#------------------------------------------------------------------------------#
set -e

# minor version. e.g. "5.7"
AM_VERSION_MINOR=${AM_VERSION_MINOR:-""}

# fixed 
ALPINE_VERSION=3.15.4
GRASS_VERSION=8.0.1
R_VERSION=4.1.3
R_PACKAGES_DATE=2022-04-01
NAME="accessmod_base"
REPO="fredmoser"
TAG="${REPO}/${NAME}:${AM_VERSION_MINOR}"
PROD=""
LOCAL=""
TEST=""
DRY="true"
BUILDERNAME=am_builder

usage() { 
  echo "Usage: AM_VERSION_MINOR=X.X $0 [-p build + push ] [-l build local] [-t build local + target test stage] [-a actually do it]" 1>&2; exit 1; 
}

while getopts "hplta" opt; do
  case "$opt" in
    h|\?)
      usage
      ;;
    p)
      PROD="true"
      ;;
    l)
      LOCAL="true"
      ;;
    t)
      TEST="true"
      ;;
    a)
      DRY=""
      ;;
  esac
done

#------------------------------------------------------------------------------#
#  Stop if not local, prod or test set 
#------------------------------------------------------------------------------#
if [[ -z "$PROD" ]] && [[ -z "$LOCAL" ]] && [[ -z "$TEST" ]]
then
  usage
  exit
fi

#------------------------------------------------------------------------------#
#  Test
#------------------------------------------------------------------------------#
if [[ -n "$TEST" ]] 
then 
  TAG="${REPO}/${NAME}:test"
  echo "Build $TAG locally, stop at test stage, tag = test"
  if [[ -n "$DRY" ]]
  then
    echo "[dry]"
  else
    docker buildx use default
    docker build \
      --build-arg ALPINE_VERSION=$ALPINE_VERSION \
      --build-arg GRASS_VERSION=$GRASS_VERSION \
      --build-arg R_PACKAGES_DATE=$R_PACKAGES_DATE \
      --target test \
      --tag ${TAG} .
  fi
  exit 0
fi

#------------------------------------------------------------------------------#
#  Non test : require minor version set 
#------------------------------------------------------------------------------#
if [[ -z "$AM_VERSION_MINOR" ]]
then
  echo -e "AM_VERSION_MINOR not set. Example:\n\nAM_VERSION_MINOR=5.7 $0 -l"
  exit
fi


#------------------------------------------------------------------------------#
#  Build locally  
#------------------------------------------------------------------------------#
if [[ -n "$LOCAL" ]] 
then 
  echo "Build $TAG locally"
  if [[ -n "$DRY" ]]
  then
    echo "[dry]"
  else
    docker buildx use default
    docker buildx build \
      --progress plain \
      --build-arg ALPINE_VERSION=$ALPINE_VERSION \
      --build-arg GRASS_VERSION=$GRASS_VERSION \
      --build-arg R_PACKAGES_DATE=$R_PACKAGES_DATE \
      --tag ${TAG} .
  fi
  exit 0
fi

#------------------------------------------------------------------------------#
#  Build multi arch + push (require login) 
#------------------------------------------------------------------------------#
if [[ -n "$PROD" ]]
then 
  echo "Build multiarch $TAG and push"

  if [[ -n "$DRY" ]]
  then
    echo "[dry]"
  else
    NBUILDER=$(docker buildx ls | grep $BUILDERNAME | wc -l)
    if [[ $NBUILDER -eq 0 ]]
    then 
      docker buildx create --name $BUILDERNAME
    else
      echo "Builder $BUILDERNAME already exists"
    fi
    docker buildx use $BUILDERNAME 
    docker buildx build \
      --platform linux/amd64,linux/arm64 \
      --build-arg ALPINE_VERSION=$ALPINE_VERSION \
      --build-arg GRASS_VERSION=$GRASS_VERSION \
      --build-arg R_PACKAGES_DATE=$R_PACKAGES_DATE \
      --push \
      --tag ${TAG} .
  fi
fi
