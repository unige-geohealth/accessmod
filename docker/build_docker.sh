#!/bin/bash 
#------------------------------------------------------------------------------#
#
#  Build AccessMod Image 
#  (c) unige.ch 
#  
#------------------------------------------------------------------------------#
set -e

# Script context should be project root, as Dockerfile requires COPY . .
DIR=$(dirname $0)
SDIR=$(cd $DIR && pwd)
CDIR=$(pwd)

get_docker_socket() {
  if [ -n "$DOCKER_HOST" ]; then
    echo "$DOCKER_HOST"
  elif [ -S "/Users/$USER/.docker/run/docker.sock" ]; then
    echo "unix:///Users/$USER/.docker/run/docker.sock"
  elif [ -S "/var/run/docker.sock" ]; then
    echo "unix:///var/run/docker.sock"
  else
    echo "Could not determine Docker socket location" >&2
    exit 1
  fi
}

DOCKER_SOCKET=$(get_docker_socket)
if [ $? -ne 0 ]; then
  exit 1
fi

# Then use it if needed
if [ "$DOCKER_SOCKET" != "$DOCKER_HOST" ]; then
  export DOCKER_HOST="$DOCKER_SOCKET"
fi

# Return to root if this script is launched from ./docker directory
if [[ $SDIR == $CDIR ]]
then
  cd ..
fi

# Version
if [[ -e version.txt ]]
then
  AM_VERSION=$(cat version.txt)
else 
  echo  "version.txt not found"
  exit 1
fi


# Set ressource path
PATHDOCKERFILE="$SDIR/Dockerfile"
#PATHEXPORTDIR="$SDIR/exported"

# fixed 
NAME="accessmod"
REPO="fredmoser"
TAG="${REPO}/${NAME}:${AM_VERSION}"
TAG_LATEST="${REPO}/${NAME}:latest" 
PROD=""
LOCAL=""
DRY="true"
BUILDERNAME=am_builder

usage() { 
  echo "Usage: $0 [-p build + push ] [-l build local] [-a actually do it]" 1>&2; exit 1; 
}

while getopts "hpla" opt; do
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
    a)
      DRY=""
      ;;
  esac
done

#------------------------------------------------------------------------------#
#  Stop if not local or prod  set 
#------------------------------------------------------------------------------#
if [[ -z "$PROD" ]] && [[ -z "$LOCAL" ]]
then
  usage
  exit
fi

#------------------------------------------------------------------------------#
# Require version set 
#------------------------------------------------------------------------------#
if [[ -z "$AM_VERSION" ]]
then
  echo -e "AM_VERSION not set. Check that this script is applied \
    from project root: version.txt file required"
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
      --file ${PATHDOCKERFILE} \
      --tag ${TAG} \
      --tag ${TAG_LATEST} .
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
      # Add --driver docker-container to ensure proper context handling
      docker buildx create --name $BUILDERNAME --driver docker-container
    else
      echo "Builder $BUILDERNAME already exists"
    fi
    # Remove the explicit context switch
    # docker buildx use $BUILDERNAME 
    docker buildx build \
      --builder $BUILDERNAME \
      --file ${PATHDOCKERFILE} \
      --platform linux/amd64,linux/arm64 \
      --push \
      --tag ${TAG} \
      --tag ${TAG_LATEST} .
  fi
fi

