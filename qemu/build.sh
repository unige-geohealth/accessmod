#!/bin/sh
set -e

# Source configuration
. ./config.sh

# Parse arguments
ARCH=${1:-x86_64}

# Create build directory
mkdir -p "$BUILD_DIR"

# Load base packages
PACKAGES=$(cat packages)

# Add architecture-specific packages and repositories
if [ "$ARCH" = "aarch64" ]; then
    PACKAGES="$PACKAGES linux-virt@edge"
    echo "https://dl-cdn.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories
    echo "https://dl-cdn.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories
    apk update
fi

# Copy config for provisioning
mkdir -p /tmp/scripts
cp scripts/provision.sh /tmp/scripts/
cp config.sh /tmp/

echo "Creating Alpine Linux VM image for $ARCH..."
alpine-make-vm-image \
    --arch $ARCH \
    --image-format vdi \
    --image-size "${DISK_SIZE}M" \
    $([ "$ARCH" = "aarch64" ] && echo "--branch edge") \
    --repositories-file /etc/apk/repositories \
    --packages "$PACKAGES" \
    --fs-skel-dir fs \
    --fs-skel-chown root:root \
    --script-chroot \
    "${BUILD_DIR}/${VM_NAME}-${VM_VERSION}-${ARCH}.vdi" << EOF
    # Run provisioning script
    sh /tmp/scripts/provision.sh

    # Clean up
    rm -rf /tmp/scripts /tmp/config.sh
EOF

# Clean up temporary files
rm -rf /tmp/scripts /tmp/config.sh

echo "VM image created successfully at ${BUILD_DIR}/${VM_NAME}-${VM_VERSION}.vdi"
