FROM osgeo/gdal:alpine-ultrasmall-latest as base

FROM base as build_grass

#
# GRASS
#
WORKDIR /tmp/grass/
ENV GRASS_VERSION="7.8.3"
ENV NUMTHREADS=2
ENV GRASS_DEPS="\
  zstd \
  zstd-libs \
  proj \
  proj-datumgrid \
  proj-util \
  sqlite \
  sqlite-libs \
  python3 \
  py3-numpy \
  py3-pillow \
  py3-six"
ENV GRASS_DEPS_BUILD="\
  tar \
  gfortran \
  autoconf \
  doxygen \
  build-base \
  bison \
  flex-dev \
  fftw-dev \
  geos-dev \
  gdal-dev \
  proj-dev \
  libpng-dev \
  tiff-dev \
  sqlite-dev \
  gsl-dev \
  zstd-dev \
  py3-numpy-dev"
COPY grass/Makefile Makefile 
COPY grass/r.walk.accessmod r.walk.accessmod
COPY script/build_grass_alpine.sh .
RUN sh build_grass_alpine.sh

FROM base as build_app
#
# APP
#
WORKDIR /tmp/app/
COPY script/build_r_alpine.sh .
ENV R_SYS_BUILD_DEPS="alpine-sdk R-dev libpq libressl-dev libxml2-dev"
ENV R_SYS_DEPS="R libxml2 fftw"
ENV R_PACKAGES_DATE="2020-07-20"
ENV R_PACKAGES_BUILD="c('BH')"
ENV R_PACKAGES="c( \
'parallel', \
'tools', \
'memoise', \
'geojsonio', \
'stringr', \
'shiny', \
'rgeos', \
'gdalUtils', \
'rgrass7', \
'rio', \
'RSQLite', \
'R.utils', \
'data.table', \
'raster', \
'maps', \
'plyr', \
'dplyr', \
'shinydashboard', \
'leaflet', \
'xml2' \
)"

RUN sh build_r_alpine.sh

#
# FINAL
#
FROM base as final
COPY --from=build_grass /usr/local/bin/grass /usr/local/bin/grass
COPY --from=build_grass /usr/local/grass* /usr/local/grass/

RUN ln -s /usr/local/grass /usr/local/grass7 && \
    ln -s /usr/local/grass $(grass --config path)

ENV GRASSBIN="/usr/local/bin/grass" \
    GRASS_SKIP_MAPSET_OWNER_CHECK=1 \
    SHELL="/bin/bash" \
    LC_ALL="en_US.UTF-8"


VOLUME /app/dev
VOLUME /app/prod
VOLUME /app/shared

ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV ACCESSMOD_DOCKER="yes"
ENV SHINY_PORT=3030
ENV SHINY_PORT_DEV=3434
ENV SHINY_PATH_APP="/app/prod"

EXPOSE 3030
EXPOSE 3434

COPY . /app/

COPY script/build_run_file.sh .
RUN sh build_run_file.sh

CMD ["Rscript", "run.R"]

