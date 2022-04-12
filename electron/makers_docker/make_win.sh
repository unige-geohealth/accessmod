#!/bin/bash
TMPDIR=/tmp/am5_build

CMD=$1;
IMG="fredmoser/electron-forge:latest"
mkdir -p $TMPDIR/node_modules
touch $TMPDIR/yarn.lock

if [[ ! -e  $(which docker) ]]; then
  echo "No docker found, install it ?" 
  exit 1;
fi

if [[ "$(docker images -q $IMG 2> /dev/null)" == "" ]]; then
  echo "No image found, build it first using ./build.sh "
  exit 1;
fi

if [[ ! -f package.json ]]; then
  echo "This script should be run in the same dir as package.json : app files are needed "
  exit 1;
fi


#
# Share app file, package.json and set folder to keep the cache
#
docker run --rm \
  -v $(pwd)/app:/build_context/app \
  -v $(pwd)/package.json:/build_context/package.json \
  -v $(pwd)/out:/build_context/out \
  -v /tmp/am5_build/node_modules:/build_context/node_modules \
  -v /tmp/am5_build/yarn.lock:/build_context/yarn.lock \
  $IMG \
  /bin/bash -c '\
  echo "Script to run by yarn in container: $CMD" \
  && yarn install\
  && yarn run '${CMD}

#
# NOTE @CMD is the command set in paackage.json e.g. make:win 
#

#docker run -ti --rm \
  #-v $(pwd)/app:/build_context/app \
  #-v $(pwd)/package.json:/build_context/package.json \
  #-v $(pwd)/out:/build_context/out \
  #-v /tmp/am5_build/node_modules:/build_context/node_modules \
  #-v /tmp/am5_build/yarn.lock:/build_context/yarn.lock \
  #fredmoser/electron-forge:latest \
  #/bin/bash 


