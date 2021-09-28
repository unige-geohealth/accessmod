# ===============
# Build am5 : gdal, grass, r + app
# ==============
ARG ALPINE_VERSION="3.12.1"
# use latest
FROM fredmoser/accessmod_grass as grass
FROM fredmoser/accessmod_r as r
FROM alpine:${ALPINE_VERSION} as merger
LABEL maintainer="frederic.moser@unige.ch"


#
# Import R
#
COPY --from=r . .


##
## Import GRASS
##
COPY --from=grass . . 

#
# GRASS env
#
ENV AM5_GRASS_RC_FILES="/am5_grass_rc" \
    GISRC="" \
    GRASSBIN="/usr/local/bin/grass" \
    GRASS_SKIP_MAPSET_OWNER_CHECK=1 \
    GISBASE="/usr/local/grass78" \
    GISDBASE="/data/dbgrass" 
# New block, as GISBASE needed
ENV LD_LIBRARY_PATH="$GISBASE/lib" \
    GRASS_LD_LIBRARY_PATH="$LD_LIBRARY_PATH" \
    PYTHONPATH="$GISBASE/etc/python:$PYTHONPATH" \
    GRASS_PYTHON="python3" \
    PATH="$GISBASE/bin:$GISBASE/scripts:$PATH" \
    SHELL="/bin/bash"

#
# R env
#
ENV _R_SHLIB_STRIP_="true" \
    TZ="UTC" \
    LC_ALL="en_US.UTF-8" \
    R_ZIPCMD="/usr/bin/zip"

#
# Demo data
#
WORKDIR /data
COPY ./config/data/demo.zip demo.zip
RUN unzip demo.zip 

#
# Copy am5 code in /app
#
WORKDIR /app
COPY . .

VOLUME /data/dbgrass
VOLUME /data/logs
VOLUME /data/cache

CMD ["Rscript","--vanilla","run.r", "3939"]
