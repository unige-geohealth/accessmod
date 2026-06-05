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

setup_dns() {
    log "Setting up DNS..."

    cat > /etc/resolv.conf << EOF
nameserver 1.1.1.1
nameserver 8.8.8.8
EOF

    mkdir -p /etc/local.d
    cat > /etc/local.d/accessmod_dns.start << EOF
#!/bin/sh
cat > /etc/resolv.conf << RESOLV
nameserver 1.1.1.1
nameserver 8.8.8.8
RESOLV
EOF
    chmod +x /etc/local.d/accessmod_dns.start

    if [ -d /etc/udhcpc ]; then
        grep -q '^RESOLV_CONF=' /etc/udhcpc/udhcpc.conf 2>/dev/null \
            && sed -i 's|^RESOLV_CONF=.*|RESOLV_CONF="no"|' /etc/udhcpc/udhcpc.conf \
            || echo 'RESOLV_CONF="no"' >> /etc/udhcpc/udhcpc.conf
    fi
}


setup_docker() {
    log "Setting up Docker..."
    rc-update add docker boot
    rc-update add local default
}

setup_arm_uefi_bootloader() {
    if [ "$(apk --print-arch)" != "aarch64" ] || [ ! -f /boot/startup.nsh ]; then
        return
    fi

    log "Setting up ARM UEFI bootloader..."

    apk add grub-efi

    grub-install \
        --target=arm64-efi \
        --efi-directory=/boot \
        --bootloader-id=alpine \
        --removable \
        --no-nvram

    mkdir -p /boot/grub
    boot_args=$(cat /boot/startup.nsh)
    kernel=${boot_args%% *}
    boot_args=${boot_args#* }
    initrd=${boot_args#initrd=}
    initrd=${initrd%% *}
    kernel_args=${boot_args#* }

    cat > /boot/grub/grub.cfg << EOF
set default=0
set timeout=0

menuentry "Alpine Linux" {
    linux /${kernel} ${kernel_args}
    initrd /${initrd}
}
EOF
}
#
# Environment setup
#
setup_environment() {
    log "Setting up environment..."

    # Set default shell to bash 
    sed -i 's|/bin/sh|/bin/bash|' /etc/passwd

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
export AM5_ARCHIVE_PATH=${AM5_ARCHIVE_PATH}

# Add menu alias and auto-start
alias menu='/bin/bash \${AM5_SCRIPTS_FOLDER}/menu_init.sh'

# Auto-start menu on login
if [ "\$TERM" != "dumb" ]; then
    menu
fi
EOF
    chmod +x /etc/profile.d/am5_env.sh
    # Record the application version for the menu
    echo "${AM5_VERSION}" > "${AM5_VERSION_FILE}"
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
    setup_dns
    setup_arm_uefi_bootloader
    setup_docker
    setup_environment
    cleanup
    log "Provisioning completed successfully"
}

main
