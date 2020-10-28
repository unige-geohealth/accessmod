# ===============
# Build am5 : R 
# ===============
FROM rhub/r-minimal as r
FROM fredmoser/am5_common as build_r 
COPY --from=r  . .

WORKDIR /src

# Notes:
# - Variables should be set again in final or after a COPY step.
# - Common dependencies are set in am5_common image, here only tempoary
#   dependencies are set.
ENV _R_SHLIB_STRIP_=true
ENV TZ=UTC
ENV R_SYS_BUILD_DEPS="build-base \
    autoconf=2.69-r2 \
    automake=1.16.1-r0 \
    R-dev \
    libressl-dev \
    libxml2-dev \
    pcre2-dev \
    m4 \
    geos-dev \
    sqlite-dev \
    proj-dev \
    linux-headers"

ENV R_NCPU=2

# Install system deps
RUN apk add --no-cache\
  --repository "http://nl.alpinelinux.org/alpine/v3.11/main" \
  $R_SYS_BUILD_DEPS

# Install R packages
RUN mkdir -p ~/.R \
   && echo "CFLAGS=-D__USE_MISC" > ~/.R/Makevars

# Custom install function
ENV R_PACKAGES_DATE="2020-10-25"
ENV R_REPO="https://mran.microsoft.com/snapshot/"${R_PACKAGES_DATE}
COPY rinstall.sh /usr/local/bin/rinstall

# Install step by step
RUN rinstall Rcpp 
RUN rinstall BH
RUN rinstall tidyverse/readxl github
RUN rinstall httpuv
RUN rinstall shiny
RUN rinstall memoise
RUN rinstall stringr
RUN rinstall rgeos
RUN rinstall gdalUtils
RUN rinstall rgrass7
RUN rinstall rio
RUN rinstall RSQLite
RUN rinstall R.utils
RUN rinstall data.table
RUN rinstall raster
RUN rinstall maps
RUN rinstall plyr
RUN rinstall dplyr
RUN rinstall shinydashboard
RUN rinstall leaflet
RUN rinstall xml2
RUN rinstall stringi
# Remove packages not used in final
RUN rinstall BH remove
RUN rinstall remotes remove

# No need for doc
RUN rm -rf /usr/local/docs/html/*

#
# Final step, merge layers
#
FROM fredmoser/am5_common as final

ENV _R_SHLIB_STRIP_=true
ENV TZ=UTC

COPY --from=build_r /usr/local/bin/R /usr/local/bin/R
COPY --from=build_r /usr/local/bin/Rscript /usr/local/bin/Rscript
COPY --from=build_r /usr/local/lib/R /usr/local/lib/R

CMD ["R"]
