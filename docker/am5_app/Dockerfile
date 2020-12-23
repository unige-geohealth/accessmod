# ===============
# Build am5 : gdal, grass, r + app
# ==============
ARG ALPINE_VERSION="3.12.1"
FROM fredmoser/am5_grass as grass
FROM fredmoser/am5_r as r
FROM alpine:${ALPINE_VERSION} as merger

LABEL maintainer="frederic.moser@unige.ch"

#
# Import GRASS
#
COPY --from=grass . . 
#/usr/local/bin/grass /usr/local/bin/grass
#COPY --from=grass /usr/local/grass78 /usr/local/grass78

#
# Import R
#
COPY --from=r . .

#
# Missing packages, to be added in app_r
#
RUN apk add --no-cache \
     zip \
     file 

#
# Missing packages to be added in app_grass
#
RUN apk add --no-cache \
     fftw 


FROM alpine:${ALPINE_VERSION} as final

COPY --from=merger . .

ENV GRASSBIN="/usr/local/bin/grass" \
    GRASS_SKIP_MAPSET_OWNER_CHECK=1 \
    SHELL="/bin/bash" \
    _R_SHLIB_STRIP_=true \
    TZ=UTC \
    LC_ALL="en_US.UTF-8"

#
# 
#
WORKDIR /app

COPY . .

VOLUME /data/dbgrass
VOLUME /data/logs
VOLUME /data/cache

CMD ["Rscript","--vanilla","run.r", "3939"]
