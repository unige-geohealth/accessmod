ARG R_VERSION=4.1.3
ARG GRASS_VERSION=8.0.2
ARG R_PACKAGES_DATE=2022-04-01
ARG GDAL_VERSION=3.6.3

# Based on:
# https://github.com/OSGeo/grass/blob/releasebranch_8_0/docker/alpine/Dockerfile 
# https://github.com/mundialis/docker-grass-gis/blob/master/Dockerfile
# https://github.com/r-hub/r-minimal/blob/master/Dockerfile

#------------------------------------------------------------------------------#
# Common : r, gdal, proj  
#------------------------------------------------------------------------------#
FROM osgeo/gdal:alpine-small-${GDAL_VERSION} as gdalproj
FROM rhub/r-minimal:${R_VERSION} as common

ARG GDAL_VERSION

LABEL authors="Frederic Moser"
LABEL maintainer="frederic.moser@unige.ch"

# https://grasswiki.osgeo.org/wiki/Compile_and_Install#Alpine_Linux
RUN echo "Install GRASS runtime" &\
      apk add --no-cache \
      bash \
      bison \
      bzip2 \
      cairo \
      fftw \
      flex \
      freetype \
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
      libpq \
      musl \
      musl-utils \
      ncurses \
      openjpeg \
      openblas \
      python3 \
      py3-numpy \
      py3-pillow \
      py3-six \
      sqlite \
      sqlite-libs \
      tiff \
      wget \
      zstd \
      zstd-libs
      
RUN echo "(re) Install R runtime deps" &\
      apk add --no-cache \
      libgfortran \
      xz-libs \
      libcurl \
      libpcrecpp \
      libbz2 \
      pcre2 \
      make \
      readline

RUN echo "Install accessmod runtime dep" &\
      apk add --no-cache \
      libpng \
      file \
      libxml2 \
      sqlite \
      geos \
      tar \
      zip \
      unzip \
      ca-certificates

COPY --from=gdalproj /usr/lib/ /usr/lib/
COPY --from=gdalproj /usr/bin/ /usr/bin/
COPY --from=gdalproj /usr/include/ /usr/include/
COPY --from=gdalproj /usr/share/proj/ /usr/share/proj/
COPY --from=gdalproj /usr/share/gdal/ /usr/share/gdal/

#
# Fix libgdal 
# Note: if compile 3.5, grass looks for so.30, if 3.4, so.28  
#
RUN ln -s /usr/lib/libgdal.so."$GDAL_VERSION" /usr/lib/libgdal.so & \
    ln -s /usr/lib/libgdal.so /usr/lib/libgdal.so.28 

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
    apk add --no-cache  -t _grass_dev \
      build-base \
      bzip2-dev \
      cairo-dev \
      fftw-dev \
      freetype-dev \
      g++ \
      gcc \
      geos-dev \
      gnutls-dev \
      libc6-compat \
      libjpeg-turbo-dev \
      libpng-dev \
      make \
      openjpeg-dev \
      openblas-dev \
      python3-dev \
      py3-setuptools \
      py3-numpy-dev \
      sqlite-dev \
      tiff-dev \
      zstd-dev
       
# set configuration options, without wxGUI
ENV GRASS_CONFIG="\
      --enable-largefile \
      --with-cxx \
      --with-proj-share=/usr/share/proj \
      --with-gdal=/usr/bin/gdal-config \
      --with-geos \
      --with-sqlite \
      --with-bzlib \
      --with-zstd \
      --with-cairo --with-cairo-ldflags=-lfontconfig \
      --with-fftw \
      --without-x \
      --without-pdal \
      --without-postgres \
      --without-wxwidgets \
      --without-openmp \
      --without-freetype \
      --without-opengl \
      --without-nls \
      --without-mysql \
      --without-ffmpeg \
      --without-odbc \
      "

# Build r.walk.accessmod
COPY modules/r.walk.accessmod r.walk.accessmod

# Patch GRASS
COPY patches .

# Configure compile and install GRASS GIS
RUN echo "Configure and compile GRASS" && \
    CFLAGS_X86="--march=x86-64 -m64" && \
    CFLAGS_ARM="--march=armv8.5-a -mcpu=apple-a14" && \
    CFLAGS="-O3 -std=gnu99 " && \
    if test $(arch) = "aarch64"; then \
    CFLAGS="$CFLAGS $CFLAGS_ARM"; \
    else \ 
    CFLAGS="$CFLAGS $CFLAGS_X86"; \
    fi &&\
    LDFLAGS="-s -Wl,--no-undefined -lblas" &&\
    LD_LIBRARY_PATH="/usr/local/lib" && \
    CXXFLAGS="$MYCXXFLAGS" &&\
    ./configure $GRASS_CONFIG && \
    make -j $(nproc --all) && \
    make install && \
    ldconfig /etc/ld.so.conf.d && \
    cd r.walk.accessmod && \
    make MODULE_TOPDIR=/usr/local/grass80

# Reduce the image size - Remove unnecessary grass files
RUN cp /usr/local/grass80/gui/wxpython/xml/module_items.xml module_items.xml; \
    rm -rf /usr/local/grass80/demolocation; \
    rm -rf /usr/local/grass80/fonts; \
    rm -rf /usr/local/grass80/gui; \
    rm -rf /usr/local/grass80/share; \
    mkdir -p /usr/local/grass80/gui/wxpython/xml/; \
    mv module_items.xml /usr/local/grass80/gui/wxpython/xml/module_items.xml; \
    apk del _grass_dev

#------------------------------------------------------------------------------#
# Build R packages
#------------------------------------------------------------------------------#
FROM common as build_r

ARG R_PACKAGES_DATE

#
# If not using proj + gdal libs from common, gdal-dev proj-dev 
#
RUN echo "Install R dev packages" && \
      apk add --no-cache  -t _r_dev \
      linux-headers \
      build-base \
      R-dev \
      libressl-dev \
      libxml2-dev \
      geos-dev \
      tiff-dev \
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
rep['CRAN'] <- 'https://packagemanager.posit.co/cran/$R_PACKAGES_DATE';\
options(Ncpus = $(nproc --all));\
options(repos = rep); \
install = function(pkg,...){ \
  install.packages(pkg,...); \
  tryCatch(library(pkg,character.only=T), \
      error = function(e){ \
      print(e);\
      quit('no',status=1);\
      })\
}" > .Rprofile

# Utils
RUN Rscript -e 'install("remotes")'
RUN Rscript -e 'install("foreign")' 
RUN Rscript -e 'install("R.utils")'
RUN Rscript -e 'install("memoise")'
RUN Rscript -e 'install("parallel")' # base package: make sure it loads
RUN Rscript -e 'install("future")'
RUN Rscript -e 'install("promises")'
RUN Rscript -e 'install("R6")'
RUN Rscript -e 'install("stevedore")'
RUN Rscript -e 'install("semver")'

# Shiny / UI / widget
RUN Rscript -e 'install("httpuv")'
RUN Rscript -e 'install("shiny")'
RUN Rscript -e 'install("shinydashboard")'
RUN Rscript -e 'install("leaflet")'

# Data i/o 
# see https://github.com/tidyverse/readxl/pull/687
RUN Rscript -e 'remotes::install_github("struckma/readxl")'
RUN Rscript -e 'install("xml2")'
RUN Rscript -e 'install("RSQLite")'
RUN Rscript -e 'install("rio")'
RUN Rscript -e 'install("pool")'

# Data manipulation
RUN Rscript -e 'install("data.table")'
RUN Rscript -e 'install("plyr")'
RUN Rscript -e 'install("dplyr")' #NOTE: do not use with plyr?..
RUN Rscript -e 'install("stringr")'

# Geospatial
RUN Rscript -e 'install("rgdal")'
RUN Rscript -e 'install("gdalUtils")'
RUN Rscript -e 'install("sp")'
RUN Rscript -e 'install("rgeos")'
RUN Rscript -e 'install("raster")'
RUN Rscript -e 'install("rgrass7")'


# Github version : fix data corruption issue during upload (https://github.com/rstudio/shiny/issues/3429) 
#RUN Rscript -e ' remotes::install_github("r-lib/later")'

# Version management

# Clean 
RUN Rscript -e 'remove.packages("remotes")' & \
    apk del _r_dev  

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
    find /usr/local/grass/docs/ -name '*.png' -exec sh -c '> {}' \; &&\
    rm -rf /var/cache/apk/* && \
    find /usr/local/lib/R/library/ -type f -path '*/include/*' -name '*.h' -delete && \
    find /usr/local/lib/R/library/ -type f -path '*/include/*' -name '*.hpp' -delete && \
    find /usr/local/lib/R/library/ -type f -path '*/po/*' -name '*.mo' -delete && \
    find /usr/local/lib/R/library/ -type f -path '*/doc/*' -name '*.pdf' -delete;

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
FROM scratch as final

COPY --from=clean . .

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
    GISBASE="/usr/local/grass"

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

#
# ⚠️  To be added in common 
#

# required by rio/stringi
RUN apk add 'icu-libs'

WORKDIR /app

CMD $GRASSBIN --version
