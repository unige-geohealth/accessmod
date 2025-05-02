#!/bin/sh
set -e

# Source configuration
. ./config.sh

# Parse arguments
ARCH=${1:-x86_64}

# Create build directory
mkdir -p "$BUILD_DIR"

# Check if alpine-make-vm-image is installed
if ! command -v alpine-make-vm-image >/dev/null 2>&1; then
    echo "Error: alpine-make-vm-image is not installed"
    echo "Please install it with: apk add alpine-make-vm-image"
    exit 1
fi

# Load base packages
PACKAGES=$(cat packages)

# Add architecture-specific packages and repositories
if [ "$ARCH" = "aarch64" ]; then
    PACKAGES="$PACKAGES linux-virt@edge"
    echo "https://dl-cdn.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories
    echo "https://dl-cdn.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories
    apk update
fi

echo "Creating Alpine Linux VM image for $ARCH..."
alpine-make-vm-image \
    --arch $ARCH \
    --image-format vdi \
    --image-size "${DISK_SIZE}M" \
    $([ "$ARCH" = "aarch64" ] && echo "--branch edge") \
    --repositories-file /etc/apk/repositories \
    --packages "$PACKAGES" \
    --script-chroot \
    "${BUILD_DIR}/${VM_NAME}-${VM_VERSION}-${ARCH}.vdi" << EOF
    # Set root password
    echo "root:${SSH_PASSWORD}" | chpasswd

    # Enable SSH password authentication
    echo 'PasswordAuthentication yes' >> /etc/ssh/sshd_config
    echo 'PubkeyAuthentication yes' >> /etc/ssh/sshd_config
    
    # Add community repository
    echo "${ALPINE_REPO}" >> /etc/apk/repositories

    # Create user and configure sudo
    adduser ${USERNAME} -D -G wheel
    addgroup ${USERNAME} docker
    echo "${USERNAME}:${USERPASSWORD}" | chpasswd
    echo "%wheel ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/wheel
    chmod 0440 /etc/sudoers.d/wheel

    # Copy scripts
    mkdir -p ${AM5_SCRIPTS_FOLDER}
    cp -r ${SCRIPTS_DIR}/* ${AM5_SCRIPTS_FOLDER}/
    chown -R ${USERNAME} ${AM5_SCRIPTS_FOLDER}
    mv ${AM5_SCRIPTS_FOLDER}/inittab /etc/inittab
    chmod +x ${AM5_SCRIPTS_FOLDER}/start.sh
    chmod +x ${AM5_SCRIPTS_FOLDER}/menu_init.sh

    # Configure services
    rc-update add docker boot
    rc-update add local default
    rc-update add sshd

    # Create Docker volumes
    docker volume create am_data_cache
    docker volume create am_data_logs
    docker volume create am_data_grass

    # Pull base image
    docker pull ${AM5_REPO}:${AM5_VERSION_LATEST}
    AM5_VERSION=\$(docker run --rm ${AM5_REPO}:${AM5_VERSION_LATEST} cat version.txt)

    # Save version
    echo \$AM5_VERSION > ${AM5_VERSION_FILE}
    chown ${USERNAME} ${AM5_VERSION_FILE}

    # Set environment
    ENV_SCRIPT=/etc/profile.d/am5_env.sh
    cat > \$ENV_SCRIPT << 'ENVEOF'
#!/bin/bash
export AM5_NAME=accessmod
export AM5_VERSION=\$AM5_VERSION
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
alias menu='sh ${AM5_SCRIPTS_FOLDER}/menu_init.sh'
menu
ENVEOF
    chmod +x \$ENV_SCRIPT

    # Set motd
    echo 'Welcome to AccessMod Alpine' > /etc/motd

    # Clear disk space
    dd if=/dev/zero of=/fill bs=1M count=\$(df -m / | tail -n1 | awk '{print \$3}') 2>/dev/null || true
    rm -f /fill

    # Mark as ready
    touch ${AM5_SCRIPTS_FOLDER}/ready
EOF

echo "VM image created successfully at ${BUILD_DIR}/${VM_NAME}-${VM_VERSION}.vdi"
