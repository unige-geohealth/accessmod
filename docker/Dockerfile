FROM fredmoser/accessmod_base:5.8-e
LABEL maintainer="frederic.moser@unige.ch"
WORKDIR /app

# To add in main build
RUN echo "Ensures that busybox zip and unzip are removed" && \
      apk del zip unzip && \
      apk add --no-cache \
          zip \
          unzip

# Copy context is expected to be app root
COPY . .

ENTRYPOINT ["/usr/local/bin/update_tz"]

CMD shiny-manager run.r $AM5_PORT_APP --title AccessMod
