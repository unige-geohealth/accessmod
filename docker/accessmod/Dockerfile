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
    AM5_PORT_HTTP=5000 \
    AM5_PORT_HTTP_PUBLIC=5080 \
    AM5_PORT_APP=3000 \
    AM5_PORT_APP_PUBLIC=3080 \
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
RUN mkdir -p /data/dbgrass \
    mkdir -p /data/logs \
    mkdir -p /data/cache 
# Second fallback in amInitData.R
ADD ./config/data/demo /data/dbgrass/demo

#
# Copy am5 code in /app
#
WORKDIR /app
COPY . .

CMD Rscript --vanilla app.r
#
# NOTE: 
# A second server should be started in a separate container to handle updates / progress / stop ...
# Using the same image. This could also be launched fro the SAME container, but separation of concerns
# is preferable : logs, health check, automatic restart, etc..
# A second image could also be build, with only this command:
# CMD ["Rscript","--vanilla","http.r", $AM5_PORT_HTTP]
# However -> electron app contain an exported archive and a second image would mean a second export.
#

