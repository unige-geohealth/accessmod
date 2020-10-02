#!/bin/sh
echo "http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories 

apk add --no-cache $GRASS_DEPS 
apk add --no-cache --virtual _grass_build $GRASS_DEPS_BUILD 

wget -qO- "https://github.com/OSGeo/grass/archive/${GRASS_VERSION}.tar.gz" | tar xvz  

cp Makefile "grass-"$GRASS_VERSION"/Makefile" 
cd "grass-"$GRASS_VERSION 

CXXFLAGS="-D__MUSL__" CFLAGS="-march=native -mtune=native" LDFLAGS="-s" ./configure \
--without-opengl \
--without-wxwidgets \
--without-cairo \
--without-freetype \
--without-x \
--with-tiff \
--with-geos \
--disable-largefile \
--with-cxx \
--with-sqlite \
--without-readline \
--without-tcltk \
--without-opengl \
--with-proj-share=/usr/share/proj \
--without-x \
--without-wxwidgets

make 

make install 

# grass74, 75, 78 and not 7.8.3
GRASS_PATH_DIR=$(find /usr/local/ -type d -name "grass*" | head -1) 
cd r.walk.accessmod/ 
make -j ${NUMTHREADS} MODULE_TOPDIR="$GRASS_PATH_DIR" 
ln -s  ${GRASS_PATH_DIR} /usr/local/bin/grass 
rm -rf ${GRASS_PATH_DIR}/demolocation 
rm -rf ${GRASS_PATH_DIR}/fonts 
rm -rf * 
apk del _grass_build
