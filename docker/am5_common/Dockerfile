# ====================
# Build am5 common dependencies.
# GDAL imported from:
# https://github.com/OSGeo/gdal/blob/master/gdal/docker/alpine-ultrasmall/Dockerfile
# ====================

# VERSIONS
ARG GDAL_VERSION=3.1.3

# ====================
# COMMON
# ====================
FROM osgeo/gdal:alpine-ultrasmall-${GDAL_VERSION} as common

# AM5 runtime depedencies (or dependencies' required dependencies ...)
ENV PACKAGES="attr \
      bash \
      bison \
      bzip2 \
      cairo \
      curl \
      fftw \
      flex \
      freetype \
      gettext \
      geos \
      gnutls \
      jsoncpp \
      libbz2 \
      libexecinfo \
      libpng \
      libunwind \
      musl \
      musl-utils \
      openblas \
      py-pip \
      py3-numpy \
      py3-pillow \
      py3-six \
      proj-datumgrid \
      proj-util \
      sqlite \
      sqlite-libs \
      tiff \
      zstd \
      zstd-libs \
      # R packages deps (https://github.com/r-hub/r-minimal/blob/master/Dockerfile) \
      libxml2 \
      pcre2 \
      fftw \
      libgfortran \
      xz-libs \
      libcurl \
      libpcrecpp \
      libbz2 \
      make \
      readline \
      icu-libs"


RUN apk update && \
    apk add --no-cache $PACKAGES

CMD ["gdalinfo","--version"]
