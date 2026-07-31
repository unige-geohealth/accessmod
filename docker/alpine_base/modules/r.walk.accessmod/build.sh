#!/bin/bash
set -euo pipefail
#
# For debugging
# from container session, bash 
# 1) grass --tmp-project XY
# 2) cd docker/alpine_base/modules/r.walk.accessmod 
# 3) ./build.sh 
#
GRASS_PATH="$(grass --config path)"
rm -f "$GRASS_PATH/bin/r.walk.accessmod"

#CFLAGS="-Ofast -std=gnu99 " && \

CFLAGS_X86="-march=x86-64 -m64"
CFLAGS_ARM="-march=armv8-a"
if [ "$(arch)" = "aarch64" ]; then
  CFLAGS_ARCH="$CFLAGS_ARM"
else
  CFLAGS_ARCH="$CFLAGS_X86"
fi

export CFLAGS="-O3 -std=gnu99 $CFLAGS_ARCH"
export CXXFLAGS="-O3 -std=c++17 $CFLAGS_ARCH"
export LDFLAGS="-s -Wl,--no-undefined"
export LD_LIBRARY_PATH="/usr/local/lib"

make MODULE_TOPDIR="$GRASS_PATH"
