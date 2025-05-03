#!/bin/sh
set -e

#
# Logging helper
#
log() {
    echo "[PROVISION] $1"
}

#
# System setup
#
setup_system() {
    log "Setting up system..."

    # Add community repository
    echo "${ALPINE_REPO}" >> /etc/apk/repositories

    # Set motd
    echo 'Welcome to AccessMod Alpine' > /etc/motd
}

wait_for_docker() {
    log "Waiting for Docker to be ready..."
    for i in {1..15}; do
        if docker info &>/dev/null; then
            log "Docker is ready."
            return 0
        fi
        sleep 1
    done
    log "Docker failed to start within 15 seconds."
    return 1
}

setup_docker() {
    set -euo pipefail

    log "Setting up Docker..."

    # Configure Docker to start at boot
    rc-update add docker boot
    rc-update add local default

    # Start Docker only if not already running
    if ! docker info &>/dev/null; then
        log "Starting Docker service..."
        service docker start
    else
        log "Docker is already running."
    fi

    # Wait for Docker to be responsive (or fail)
    wait_for_docker || exit 1

    # Create Docker volumes
    docker volume create am_data_cache
    docker volume create am_data_logs
    docker volume create am_data_grass

    # Ensure required variables are set
    : "${AM5_REPO:?AM5_REPO not set}"
    : "${AM5_VERSION_LATEST:?AM5_VERSION_LATEST not set}"
    : "${AM5_VERSION_FILE:?AM5_VERSION_FILE not set}"

    # Pull base image and get version
    image=${AM5_REPO}:${AM5_VERSION_LATEST}
    log "Pulling base image ... ${image}"
    docker pull "${image}"
    AM5_VERSION=$(docker run --rm "${image}" cat version.txt)

    # Save version to file
    echo "${AM5_VERSION}" > "${AM5_VERSION_FILE}"
}
#
# Environment setup
#
setup_environment() {
    log "Setting up environment..."

    # Create environment script
    cat > /etc/profile.d/am5_env.sh << EOF
#!/bin/bash
export AM5_NAME=accessmod
export AM5_VERSION=${AM5_VERSION}
export AM5_PORT_APP=${AM5_PORT_APP}
export AM5_PORT_APP_PUBLIC=${AM5_PORT_APP_PUBLIC}
export AM5_PORT_HTTP=${AM5_PORT_HTTP}
export AM5_PORT_HTTP_PUBLIC=${AM5_PORT_HTTP_PUBLIC}
export AM5_SCRIPTS_FOLDER=${AM5_SCRIPTS_FOLDER}
export AM5_VERSION_FILE=${AM5_VERSION_FILE}
export AM5_VERSION_LATEST=${AM5_VERSION_LATEST}
export AM5_REPO=${AM5_REPO}
export AM5_HUB_API=${AM5_HUB_API}
export AM5_MIN_VERSION=${AM5_MIN_VERSION}

# Add menu alias and auto-start
alias menu='sh \${AM5_SCRIPTS_FOLDER}/menu_init.sh'

# Auto-start menu on login
if [ "\$TERM" != "dumb" ]; then
    menu
fi
EOF
    chmod +x /etc/profile.d/am5_env.sh
}

#
# Cleanup
#
cleanup() {
    log "Cleaning up..."

    # Clear disk space
    dd if=/dev/zero of=/fill bs=1M count=$(df -m / | tail -n1 | awk '{print $3}') 2>/dev/null || true
    rm -f /fill

    # Mark as ready
    touch ${AM5_SCRIPTS_FOLDER}/ready
}

#
# Main
#
main() {
    log "Starting provisioning..."
    setup_system
    setup_docker
    setup_environment
    cleanup
    log "Provisioning completed successfully"
}

main
