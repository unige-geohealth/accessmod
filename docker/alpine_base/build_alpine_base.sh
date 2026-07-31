#!/bin/bash 
#------------------------------------------------------------------------------#
#
#  Build AccessMod Base Image 
#  (c) unige.ch 
#  
#------------------------------------------------------------------------------#
set -e

# AM_VERSION_MINOR remains accepted for compatibility with existing commands.
BASE_IMAGE_TAG=${BASE_IMAGE_TAG:-${AM_VERSION_MINOR:-""}}
GRASS_VERSION=${GRASS_VERSION:-"8.5.0"}

# fixed 
NAME="accessmod_base"
REPO="fredmoser"
TAG="${REPO}/${NAME}:${BASE_IMAGE_TAG}"
PROD=""
LOCAL=""
TEST=""
DRY="true"
BUILDERNAME=am_builder
LOCAL_BUILDER=${LOCAL_BUILDER:-$(docker context show)}
TARGET_STAGE="final"
DIRBUILDCACHE="./_build_cache"

usage() {
  echo "Usage: GRASS_VERSION=X.Y.Z BASE_IMAGE_TAG=X.X-x $0 [-p build + push ] [-l build local] [-t build local + target test stage] [-s <stage> stop at stage ] [-a actually do it]" 1>&2; exit 1;
}

while getopts "hpltas:" opt; do
  case "$opt" in
    h|\?)
      usage
      ;;
    s) 
      echo "stage"$OPTARG
      TARGET_STAGE=$OPTARG
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
    docker buildx build \
      --builder "${LOCAL_BUILDER}" \
      --build-arg GRASS_VERSION="${GRASS_VERSION}" \
      --target test \
      --load \
      --tag ${TAG} .
  fi
  exit 0
fi

#------------------------------------------------------------------------------#
#  Non test : require minor version set 
#------------------------------------------------------------------------------#
if [[ -z "$BASE_IMAGE_TAG" ]]
then
  echo -e "BASE_IMAGE_TAG not set. Example:\n\nBASE_IMAGE_TAG=5.9-f $0 -l"
  exit
fi


#------------------------------------------------------------------------------#
#  Build locally  
#------------------------------------------------------------------------------#
if [[ -n "$LOCAL" ]] 
then 
  echo "Build $TAG locally"

  #if [[ -n "$TARGET_STAGE" ]]
  #then
    echo "Stop at stage $TARGET_STAGE"
  #fi

  if [[ -n "$DRY" ]]
  then
    echo "[dry]"
  else
    docker buildx build \
      --builder "${LOCAL_BUILDER}" \
      --build-arg GRASS_VERSION="${GRASS_VERSION}" \
      --progress plain \
      --target $TARGET_STAGE \
      --load \
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

    #
    # Create cache dir if not already created
    #
    if [[ ! -e $DIRBUILDCACHE ]]
    then
      echo "No cache dir found, create it"
      mkdir -p $DIRBUILDCACHE
    fi

    #
    # Create builder if required
    #
    NBUILDER=$(docker buildx ls | grep $BUILDERNAME | wc -l)
    if [[ $NBUILDER -eq 0 ]]
    then 
      docker buildx create --name $BUILDERNAME
    else
      echo "Builder $BUILDERNAME already exists"
    fi
      
    docker buildx build \
      --builder $BUILDERNAME \
      --build-arg GRASS_VERSION="${GRASS_VERSION}" \
      --cache-to=type=local,dest=./_build_cache \
      --cache-from=type=local,src=./_build_cache \
      --platform linux/amd64,linux/arm64 \
      --push \
      --tag ${TAG} .
  fi
fi
