#!/bin/sh
apk add --no-cache nodejs \
    && echo "{\"dependencies\": {\"@fxi/shiny-cluster\": \""${SHINY_CLUSTER_VERSION}"\"}}" > package.json \
    && echo "require('@fxi/shiny-cluster').run({ path : '/mnt/app', concurency : "$SHINY_CLUSTER_CONCURRENCY", port : "$SHINY_CLUSTER_PORT" });" > app.js \
    && npm install \
    && npm cache clean --force \
    && npm uninstall -g npm \
    && rm -rf $HOME/.npm

