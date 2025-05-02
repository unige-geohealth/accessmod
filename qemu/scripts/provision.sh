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

#
# User setup
#
setup_user() {
    log "Setting up user..."

    # Create user and configure sudo
    adduser ${USERNAME} -D -G wheel
    addgroup ${USERNAME} docker
    echo "${USERNAME}:${USERPASSWORD}" | chpasswd
    echo "%wheel ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/wheel
    chmod 0440 /etc/sudoers.d/wheel

    # Set proper ownership
    chown -R ${USERNAME}:${USERNAME} /home/${USERNAME}
}

#
# Docker setup
#
setup_docker() {
    log "Setting up Docker..."

    # Configure and start services
    rc-update add docker boot
    rc-update add local default
    service docker start
    sleep 10

    # Create Docker volumes
    docker volume create am_data_cache
    docker volume create am_data_logs
    docker volume create am_data_grass

    # Pull base image and get version
    log "Pulling base image ${AM5_REPO}:${AM5_VERSION_LATEST}..."
    docker pull ${AM5_REPO}:${AM5_VERSION_LATEST}
    AM5_VERSION=$(docker run --rm ${AM5_REPO}:${AM5_VERSION_LATEST} cat version.txt)

    # Save version
    echo ${AM5_VERSION} > ${AM5_VERSION_FILE}
    chown ${USERNAME}:${USERNAME} ${AM5_VERSION_FILE}
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
    setup_user
    setup_docker
    setup_environment
    cleanup
    log "Provisioning completed successfully"
}

main
