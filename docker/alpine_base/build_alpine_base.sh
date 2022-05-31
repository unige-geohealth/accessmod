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
NAME="accessmod_base"
REPO="fredmoser"
TAG="${REPO}/${NAME}:${AM_VERSION_MINOR}"
PROD=""
LOCAL=""
TEST=""
DRY="true"
BUILDERNAME=am_builder
TARGET_STAGE="final"

usage() { 
  echo "Usage: AM_VERSION_MINOR=X.X $0 [-p build + push ] [-l build local] [-t build local + target test stage] [-s <stage> stop at stage ] [-a actually do it]" 1>&2; exit 1; 
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
    docker buildx use default
    docker build \
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

  #if [[ -n "$TARGET_STAGE" ]]
  #then
    echo "Stop at stage $TARGET_STAGE"
  #fi

  if [[ -n "$DRY" ]]
  then
    echo "[dry]"
  else
    docker buildx use default
    docker build \
      --progress plain \
      --target $TARGET_STAGE \
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
      --push \
      --tag ${TAG} .
  fi
fi
