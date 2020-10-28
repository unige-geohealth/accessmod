# ===============
# Build am5 : common + grass + r
# ==============

FROM fredmoser/am5_grass as grass
FROM fredmoser/am5_r as r
FROM fredmoser/am5_common as common 

LABEL maintainer="frederic.moser@unige.ch"

ENV GRASSBIN="/usr/local/bin/grass" \
    GRASS_SKIP_MAPSET_OWNER_CHECK=1 \
    SHELL="/bin/bash" \
    _R_SHLIB_STRIP_=true \
    TZ=UTC \
    LC_ALL="en_US.UTF-8"

#
# Import GRASS
#
COPY --from=grass /usr/local/bin/grass /usr/local/bin/grass
COPY --from=grass /usr/local/grass78 /usr/local/grass78

#
# Import R
#
COPY --from=r /usr/local/bin/R /usr/local/bin/R
COPY --from=r /usr/local/bin/Rscript /usr/local/bin/Rscript
COPY --from=r /usr/local/lib/R /usr/local/lib/R

#
# 
#
WORKDIR /app

COPY ./ ./

VOLUME /data/dbgrass
VOLUME /data/logs
VOLUME /data/cache

CMD ["Rscript","--vanilla","run.R", "3939"]
