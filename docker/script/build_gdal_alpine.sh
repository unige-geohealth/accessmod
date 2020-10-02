#!/bin/sh
echo "http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories \
    && apk add --no-cache $GDAL_DEPS_RUL \
    && apk add --no-cache --virtual _build_deps $GDAL_BUILD_DEPS \
    && wget -qO- "https://download.osgeo.org/gdal/"$GDAL_VERSION"/gdal-"$GDAL_VERSION".tar.gz" | tar xvz  \
    && cd "gdal-"$GDAL_VERSION \
    && CXXFLAGS="-D__MUSL__ -Os" CFLAGS="-march=native -mtune=native -Os" LDFLAGS="-s" \
    ./configure \
    --with-geos \
    --with-geotiff=internal \
    --with-libtiff=internal \
    --with-libz=internal \
    --with-threads \
    --disable-all-optional-drivers \
    --enable-driver-shape \
    --without-jpeg12 \
    --without-pcraster \
    --without-pcidsk \
    --without-lerc \
    --without-gnm \
    --without-gif \
    --without-webp \
    --without-xml2 \
    -enable-lto \
    && make \
    && make install \
    && rm -rf /tmp/gdal \
    && apk del _build_deps

