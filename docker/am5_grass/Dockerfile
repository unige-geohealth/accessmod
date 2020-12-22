# ====================
# Build am5 : grass + modules
# Based on https://github.com/OSGeo/grass/blob/master/docker/alpine/Dockerfile_alpine
# ====================
#
# Global versions
#
ARG ALPINE_VERSION
ARG GDAL_VERSION
ARG GRASS_VERSION

FROM osgeo/gdal:alpine-small-${GDAL_VERSION} as gdal
FROM alpine:${ALPINE_VERSION} as build_grass

LABEL maintainer="frederic.moser@unige.ch"

#
# Merge gdal
#
COPY --from=gdal /usr/ /usr/

ARG GRASS_VERSION

WORKDIR /build

# DOWNLOAD SOURCE
RUN echo "Downlaod grass version ${GRASS_VERSION}" && \
    wget -O grass.tar.gz \
    https://grass.osgeo.org/grass78/source/grass-${GRASS_VERSION}.tar.gz

# CONFIG VARIABLES

# set configuration options, without wxGUI
ENV GRASS_CONFIG="\
      --enable-largefile \
      --with-cxx \
      --with-proj --with-proj-share=/usr/share/proj \
      --with-gdal=/usr/bin/gdal-config \
      --with-python \
      --with-geos \
      --with-sqlite \
      --with-bzlib \
      --with-zstd \
      --with-cairo --with-cairo-ldflags=-lfontconfig \
      --with-fftw \
      --without-postgres \
      --without-freetype \
      --without-openmp \
      --without-pdal \
      --without-opengl \
      --without-nls \
      --without-mysql \
      --without-odbc \
      --without-openmp \
      --without-ffmpeg \
      "

# Set environmental variables for GRASS GIS compilation, without debug symbols
ENV MYCFLAGS="-O2 -std=gnu99 -m64" \
    MYLDFLAGS="-s -Wl,--no-undefined -lblas" \
    # CXX stuff:
    LD_LIBRARY_PATH="/usr/lib"\
    LDFLAGS="$MYLDFLAGS" \
    CFLAGS="$MYCFLAGS" \
    CXXFLAGS="$MYCXXFLAGS" \
    NUMTHREADS=2 


# These packages are required to compile GRASS GIS.
ENV GRASS_BUILD_PACKAGES="\
      build-base \
      bzip2-dev \
      cairo-dev \
      fftw-dev \
      freetype-dev \
      flex \
      g++ \
      gcc \
      geos-dev \
      git \
      bison \
      gnutls-dev \
      libc6-compat \
      libjpeg-turbo-dev \
      libpng-dev \
      make \
      openjpeg-dev \
      readline-dev \
      openblas-dev \
      proj-dev \
      python3-dev \
      py3-numpy-dev \
      py3-six \
      sqlite-dev \
      tar \
      tiff-dev \
      unzip \
      zip \
      zstd-dev"

# Extract archive
RUN tar --extract --file grass.tar.gz --strip-components 1

# Add build packages
RUN apk add --no-cache $GRASS_BUILD_PACKAGES

# Configure compile and install GRASS GIS
RUN ./configure $GRASS_CONFIG && \
    make -j $NUMTHREADS && \
    make install && \
    ldconfig /etc/ld.so.conf.d

# Link grass: used in last COPY
RUN ln -s `find /usr/local/bin -name "grass*"` /usr/local/bin/grass

# build r.walk.accessmod (last step of build : will change)
COPY modules/r.walk.accessmod r.walk.accessmod
RUN cd r.walk.accessmod && \
     make MODULE_TOPDIR=/usr/local/grass78

# clean and preserve module_items.xml
RUN cp /usr/local/grass78/gui/wxpython/xml/module_items.xml module_items.xml; \
    rm -rf /usr/local/grass78/demolocation; \
    rm -rf /usr/local/grass78/fonts; \
    rm -rf /usr/local/grass78/gui; \
    rm -rf /usr/local/grass78/share; \
    rm -rf /usr/local/grass78/docs/html/*.png; \
    rm -rf /usr/local/grass78/docs/html/*.jpg; \
    mkdir -p /usr/local/grass78/gui/wxpython/xml/; \
    mv module_items.xml /usr/local/grass78/gui/wxpython/xml/module_items.xml;


FROM alpine:${ALPINE_VERSION} as python 

RUN apk add --no-cache \
      python3 \
      py3-six
RUN find . -name '*.pyc' -delete

FROM alpine:${ALPINE_VERSION} as final 

ENV TZ=UTC
ENV LC_ALL="en_US.UTF-8"

#
# Copy from builders
#
COPY --from=python /usr/ /usr/
COPY --from=gdal /usr/ /usr/
COPY --from=build_grass /usr/local/bin/grass /usr/local/bin/grass
COPY --from=build_grass /usr/local/grass78 /usr/local/grass78/

ENV GRASSBIN="/usr/local/bin/grass" \
    GRASS_SKIP_MAPSET_OWNER_CHECK=1 \
    SHELL="/bin/bash"

WORKDIR /grassdb
VOLUME /grassdb

CMD ["grass", "--tmp-location", "EPSG:4326" ,"--exec", "g.version"]
