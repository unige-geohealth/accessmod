ARG ALPINE_VERSION=3.15.4
ARG GRASS_VERSION=8.0.1
ARG R_VERSION=4.1.3
ARG R_PACKAGES_DATE=2022-04-01

# Based on:
# https://github.com/OSGeo/grass/blob/releasebranch_8_0/docker/alpine/Dockerfile 
# https://github.com/mundialis/docker-grass-gis/blob/master/Dockerfile
# https://github.com/r-hub/r-minimal/blob/master/Dockerfile



#------------------------------------------------------------------------------#
# Common : base for R and GRASS 
# ( r-minimal : arm + amd ok )
#------------------------------------------------------------------------------#
FROM rhub/r-minimal:${R_VERSION} as rbase
FROM alpine:${ALPINE_VERSION} as common

USER root

LABEL authors="Frederic Moser"
LABEL maintainer="frederic.moser@unige.ch"

COPY --from=rbase /usr/local /usr/local

RUN echo "Install GRASS runtime dependencies" &\
    apk update &\
    apk add --no-cache \
      bash \
      bison \
      bzip2 \
      cairo \
      fftw \
      flex \
      freetype \
      gdal \
      gdal-tools \
      gettext \
      geos \
      gnutls \
      jsoncpp \
      laszip \
      libbz2 \
      libexecinfo \
      libgeotiff \
      libjpeg-turbo \
      libpng \
      libunwind \
      musl \
      musl-utils \
      ncurses \
      openjpeg \
      openblas \
      python3 \
      py3-numpy \
      py3-pillow \
      py3-six \
      postgresql \
      proj-util \
      sqlite \
      sqlite-libs \
      subversion \
      tiff \
      wget \
      zstd \
      zstd-libs 
      
RUN echo "Install R runtime dep" &\
      apk add --no-cache \
      libpng \
      libxml2 \
      icu-libs \
      sqlite \
      geos \
      tar \
      proj \
      zip \
      unzip \
      ca-certificates \
      libgfortran \
      xz-libs \
      libcurl \
      libpcrecpp \
      libbz2 \
      pcre2 \
      make \
      readline

#------------------------------------------------------------------------------#
# Build GRASS 
#------------------------------------------------------------------------------#
FROM common as build_grass

ARG GRASS_VERSION

WORKDIR /build

# Download and extract sources
RUN echo "Download grass version ${GRASS_VERSION}" && \
    wget -O grass.tar.gz \
    https://github.com/OSGeo/grass/archive/refs/tags/${GRASS_VERSION}.tar.gz && \
    tar --extract --file grass.tar.gz --strip-components 1

# GRASS dev packages
RUN echo "Install GRASS DEV packages" && \
    apk add --no-cache \
      build-base \
      bzip2-dev \
      cairo-dev \
      fftw-dev \
      freetype-dev \
      g++ \
      gcc \
      gdal-dev \
      geos-dev \
      gnutls-dev \
      libc6-compat \
      libjpeg-turbo-dev \
      libpng-dev \
      make \
      openjpeg-dev \
      openblas-dev \
      proj-dev \
      python3-dev \
      py3-numpy-dev \
      sqlite-dev \
      tiff-dev \
      zstd-dev


# set configuration options, without wxGUI
ENV GRASS_CONFIG="\
      --enable-largefile \
      --with-cxx \
      --with-proj-share=/usr/share/proj \
      --with-gdal \
      --with-geos \
      --with-sqlite \
      --with-bzlib \
      --with-zstd \
      --with-cairo --with-cairo-ldflags=-lfontconfig \
      --with-fftw \
      --without-pdal \
      --without-postgres \
      --without-openmp \
      --without-freetype \
      --without-opengl \
      --without-nls \
      --without-mysql \
      --without-odbc \
      --without-ffmpeg \
      "

# Set environmental variables for GRASS GIS compilation, without debug symbols
ENV MYCFLAGS="-O2 -std=gnu99 --march=native" \
    MYLDFLAGS="-s -Wl,--no-undefined -lblas" \
    # CXX stuff:
    LD_LIBRARY_PATH="/usr/local/lib" \
    LDFLAGS="$MYLDFLAGS" \
    CFLAGS="$MYCFLAGS" \
    CXXFLAGS="$MYCXXFLAGS" \
    NUMTHREADS=2

# Configure compile and install GRASS GIS
RUN echo "Configure and compile grass" && \
    ./configure $GRASS_CONFIG && \
    make -j $NUMTHREADS && \
    make install && \
    ldconfig /etc/ld.so.conf.d

# Build r.walk.accessmod (last step of build : will change)
COPY modules/r.walk.accessmod r.walk.accessmod
RUN cd r.walk.accessmod && \
     make MODULE_TOPDIR=/usr/local/grass80

# Reduce the image size - Remove unnecessary grass files
RUN cp /usr/local/grass80/gui/wxpython/xml/module_items.xml module_items.xml; \
    rm -rf /usr/local/grass80/demolocation; \
    rm -rf /usr/local/grass80/fonts; \
    rm -rf /usr/local/grass80/gui; \
    rm -rf /usr/local/grass80/share; \
    mkdir -p /usr/local/grass80/gui/wxpython/xml/; \
    mv module_items.xml /usr/local/grass80/gui/wxpython/xml/module_items.xml;



#------------------------------------------------------------------------------#
# Build R packages
#------------------------------------------------------------------------------#
FROM common as build_r

ARG R_PACKAGES_DATE


RUN echo "Install R dev packages" && \
      apk add --no-cache \
      linux-headers \
      build-base \
      R-dev \
      libressl-dev \
      libxml2-dev \
      gdal-dev \
      geos-dev \
      tiff-dev \
      proj-dev \
      libpng-dev \
      pcre2-dev \
      sqlite-dev \
      m4 \
      zip \
      file \
      autoconf \
      automake 

WORKDIR /build

# Custom install function 
# - Use mran to load package at fixed date
# - STOP build if package can't be loaded
#   (instead of issuing a warning and continue )
RUN echo -e "\
rep <- getOption('repos');\
rep['CRAN'] <- 'https://mran.microsoft.com/snapshot/$R_PACKAGES_DATE';\
options(Ncpus = $(nproc --all));\
options(repos = rep); \
install = function(pkg){ \
  install.packages(pkg); \
  tryCatch(library(pkg,character.only=T), \
      error = function(e){ \
      print(e);\
      quit('no',status=1);\
      })\
}" > .Rprofile

# Utils
RUN Rscript -e 'install("foreign")' 
RUN Rscript -e 'install("R.utils")'
RUN Rscript -e 'install("memoise")'
RUN Rscript -e 'install("parallel")'
RUN Rscript -e 'install("remotes")'
RUN Rscript -e 'install("future")'
RUN Rscript -e 'install("promises")'
RUN Rscript -e 'install("R6")'
RUN Rscript -e 'install("stevedore")'
RUN Rscript -e 'install("semver")'

# Shiny / UI / widget
RUN Rscript -e 'install("httpuv")'
RUN Rscript -e 'install("shiny")'
RUN Rscript -e 'install("leaflet")'
RUN Rscript -e 'install("shinydashboard")'

# Data / geo spatial
RUN Rscript -e 'install("rgdal")'
RUN Rscript -e 'install("gdalUtils")'
RUN Rscript -e 'install("xml2")'

# see https://github.com/tidyverse/readxl/pull/687
RUN Rscript -e 'remotes::install_github("struckma/readxl")'
RUN Rscript -e 'install("RSQLite")'
RUN Rscript -e 'install("rio")'
RUN Rscript -e 'install("sp")'
RUN Rscript -e 'install("rgeos")'
RUN Rscript -e 'install("raster")'
RUN Rscript -e 'install("rgrass7")'

# Data manipulation
RUN Rscript -e 'install("data.table")'
RUN Rscript -e 'install("plyr")'
RUN Rscript -e 'install("dplyr")' #NOTE: do not use with plyr?..
RUN Rscript -e 'install("stringr")'
RUN Rscript -e 'install("pool")'


# Github version : fix data corruption issue during upload (https://github.com/rstudio/shiny/issues/3429) 
RUN Rscript -e ' remotes::install_github("r-lib/later")'

# Version management

# Clean 
RUN Rscript -e 'remove.packages("remotes")'


#------------------------------------------------------------------------------#
# CLEANING
#------------------------------------------------------------------------------#
FROM common as clean

ENV LC_ALL="en_US.UTF-8"

# Copy GRASS GIS from build image
COPY --from=build_grass /usr/local/bin/grass /usr/local/bin/grass
COPY --from=build_grass /usr/local/grass* /usr/local/grass/

# Link grass /usr/local/grass <-> /usr/local/grass80
RUN ln -sf /usr/local/grass `grass --config path`

# Copy built R libs 
COPY --from=build_r /usr/local/lib/R /usr/local/lib/R

# Remove unused file
RUN find / -name '*.pyc' -delete && \
    find /usr/local/grass/docs/ -name '*.jpg' -exec sh -c '> {}' \; &&\
    find /usr/local/grass/docs/ -name '*.png' -exec sh -c '> {}' \;

#------------------------------------------------------------------------------#
#  TESTING : check if everything is ok (requires --target = test )
#------------------------------------------------------------------------------#
FROM clean as test
RUN apk add build-base \
      g++ \
      gcc \
      make 

# Test addon installation
RUN grass --tmp-location XY --exec g.extension extension=r.flip 

# test addon description
RUN grass --tmp-location XY --exec \
    Rscript -e "library(rgrass7);parseGRASS('r.flip')"

## Load main accessmod packages
RUN Rscript -e "\
library(shiny);\
library(leaflet);\
library(rgrass7);\
library(rgeos);\
library(sp);\
library(rgdal);\
library(rio);\
library(readxl)"

RUN echo "Loading GRASS, R + dependencies seems ok"

#------------------------------------------------------------------------------#
#  FINAL : remove layers, set env
#------------------------------------------------------------------------------#
FROM alpine:${ALPINE_VERSION} as final

COPY --from=clean / /

# Don't force GRASS_OVERWRITE=1, it prevents features to be 
# appended to existing ones.
ENV AM5_PORT_HTTP=5000 \
    AM5_PORT_HTTP_PUBLIC=5080 \
    AM5_PORT_APP=3000 \
    AM5_PORT_APP_PUBLIC=3080 \
    AM5_DB_PATH="/data/dbgrass" \
    AM5_LOGS_PATH="/data/logs" \
    AM5_CACHE_PATH="/data/cache" \
    AM5_TMP_PATH="/tmp" \
    AM5_APP_PATH="/app" \
    TZ=UTC \
    LC_ALL="en_US.UTF-8" \
    _R_SHLIB_STRIP_="true" \
    R_ZIPCMD="/usr/bin/zip" \
    SHELL="/bin/bash" \
    PROJ_NETWORK="ON" \
    GISRC="" \
    GRASSBIN="/usr/local/bin/grass" \
    GRASS_SKIP_MAPSET_OWNER_CHECK=1 \
    GRASS_PYTHON="python3" \
    GISBASE="/usr/local/grass"\
    GRASS_OVERWRITE=0

# composed env
ENV GISDBASE="${AM5_DB_PATH}"
ENV LD_LIBRARY_PATH="$GISBASE/lib"
ENV GRASS_LD_LIBRARY_PATH="$LD_LIBRARY_PATH"
ENV PYTHONPATH="$GISBASE/etc/python:$PYTHONPATH"
ENV PATH=$GISBASE/bin:$GISBASE/scripts:$PATH
#
# Demo data
#
RUN mkdir -p ${AM5_DB_PATH} \
    mkdir -p ${AM5_LOGS_PATH} \
    mkdir -p ${AM5_CACHE_PATH} 

ADD data/demo ${AM5_DB_PATH}/demo

WORKDIR /app

CMD $GRASSBIN --version
