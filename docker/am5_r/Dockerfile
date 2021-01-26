#
# Global versions
# Alpine version should match 
#
ARG GDAL_VERSION="3.1.3"
ARG R_PACKAGES_DATE="2020-12-20"
ARG ALPINE_VERSION="3.12.1"
ARG R_VERSION="4.0.3"
# R SHared LIbrary strip ( r >  3.6.0 )
ARG _R_SHLIB_STRIP_=true
# Set timezone
ARG TZ=UTC

#
# R
#
FROM osgeo/gdal:alpine-small-${GDAL_VERSION} as gdal
FROM rhub/r-minimal:${R_VERSION} as build_r

LABEL maintainer="frederic.moser@unige.ch"

# Mention ARG so it's copied from global
# see https://github.com/moby/moby/issues/37345#issuecomment-400245466
ARG R_PACKAGES_DATE
ARG ALPINE_VERSION
ARG _R_SHLIB_STRIP_
ARG TZ 
# ARG TO ENV -->
ENV _R_SHLIB_STRIP_=$_R_SHLIB_STRIP_
ENV TZ=$TZ

#
# Add gdal
#
COPY --from=gdal / /

RUN echo "http://dl-cdn.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories
# R httuv -> autoconf + automake specific version -> 3.11 main
RUN echo "http://dl-cdn.alpinelinux.org/alpine/v3.11/main" >> /etc/apk/repositories

RUN apk add --no-cache \
    libpng \
    libxml2 \
    icu-libs \
    sqlite \
    geos \
    proj \
    ca-certificates

RUN apk add --no-cache --virtual build_deps \
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
  autoconf=2.69-r2 \
  automake=1.16.1-r0 

#
# Install libraries
#
WORKDIR /build

RUN echo "local({ \
rep <- getOption('repos'); \
rep['CRAN'] <- 'https://mran.microsoft.com/snapshot/$R_PACKAGES_DATE'; \
options(Ncpus = $(nproc --all)) ; \
options(repos = rep) \
})" > .Rprofile

# Utils
RUN Rscript -e 'install.packages("foreign")' # ( not working in 3.6.3 )
RUN Rscript -e 'install.packages("R.utils")'
RUN Rscript -e 'install.packages("memoise")'
RUN Rscript -e 'install.packages("parallel")'
RUN Rscript -e 'install.packages("remotes")'

# Shiny / UI / widget
RUN Rscript -e 'remotes::install_github("rstudio/httpuv")'
RUN Rscript -e 'install.packages("shiny")'
RUN Rscript -e 'install.packages("leaflet")'
RUN Rscript -e 'install.packages("shinydashboard")'

# Data / geo spatial
RUN Rscript -e 'remotes::install_github("r-forge/rgdal/pkg")'
RUN Rscript -e 'install.packages("gdalUtils")'
RUN Rscript -e 'install.packages("xml2")'
RUN Rscript -e 'remotes::install_github("tidyverse/readxl")'
RUN Rscript -e 'install.packages("RSQLite")'
RUN Rscript -e 'install.packages("rio")'
RUN Rscript -e 'install.packages("sp")'
RUN Rscript -e 'install.packages("rgeos")'
RUN Rscript -e 'install.packages("raster")'
RUN Rscript -e 'install.packages("rgrass7")'

# Data manipulation
RUN Rscript -e 'install.packages("data.table")'
RUN Rscript -e 'install.packages("plyr")'
RUN Rscript -e 'install.packages("dplyr")' #NOTE: do not use with plyr?..
RUN Rscript -e 'install.packages("stringr")'
RUN Rscript -e 'install.packages("pool")'

# Async
RUN Rscript -e 'install.packages("future")'
RUN Rscript -e 'install.packages("promises")'

# OO
RUN Rscript -e 'install.packages("R6")'


# Docker client
RUN Rscript -e  'install.packages("stevedore")'

# Clean 
RUN Rscript -e 'remove.packages("BH")'
RUN Rscript -e 'remove.packages("remotes")'

# Clean pkg. apk does not support removing package with given version (automake=version, autoconf=version..  So.. Duplicate list here
RUN apk del build_deps


FROM alpine:${ALPINE_VERSION} as final

COPY --from=build_r /usr/ /usr/

RUN apk add --no-cache \
    libgfortran \
    xz-libs \
    libcurl \
    libpcrecpp \
    libbz2 \
    pcre2 \
    make \
    readline

ARG TZ
ENV TZ=$TZ

CMD ["R", "--version"]
