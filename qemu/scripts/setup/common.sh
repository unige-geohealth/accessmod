#!/bin/sh

# Exit on error
set -e

# Source configuration if not already sourced
if [ -z "$USERNAME" ]; then
    . /tmp/config.sh
fi

# Common functions
log() {
    echo "[SETUP] $1"
}

check_required_vars() {
    for var in "$@"; do
        if [ -z "${!var}" ]; then
            log "Error: Required variable $var is not set"
            exit 1
        fi
    done
}

# Common variables
SETUP_DIR="/tmp/scripts/setup"
