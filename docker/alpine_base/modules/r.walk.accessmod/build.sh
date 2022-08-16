#!/bin/bash
#
# For debugging
# from container session, bash 
# 1) grass --tmp-location XY
# 2) cd docker/alpine_base/modules/r.walk.accessmod 
# 3) ./build.sh 
#
rm  /usr/local/grass80/bin/r.walk.accessmod
rm -rf OBJ.aarch64-unknown-linux-musl

#CFLAGS="-Ofast -std=gnu99 " && \

LOCATION_NAME=tmploc  && \
CFLAGS_X86="--march=x86-64 -m64" && \
    CFLAGS_ARM="--march=armv8.5-a -mcpu=apple-a14" && \
    CFLAGS="-std=gnu99 " && \
    if test $(arch) = "aarch64"; then \
    CFLAGS="$CFLAGS $CFLAGS_ARM"; \
    else \ 
    CFLAGS="$CFLAGS $CFLAGS_X86"; \
    fi &&\
    LDFLAGS="-s -Wl,--no-undefined -lblas" &&\
    LD_LIBRARY_PATH="/usr/local/lib" && \
    CXXFLAGS="$MYCXXFLAGS" &&\
    make MODULE_TOPDIR=/usr/local/grass80

