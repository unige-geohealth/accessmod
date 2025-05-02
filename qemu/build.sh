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
    "${BUILD_DIR}/${VM_NAME}-${VM_VERSION}-${ARCH}.vdi" -- ./scripts/provision.sh 

. ./scripts/convert_to_ova.sh 

echo "VM image created successfully at ${BUILD_DIR}/${VM_NAME}-${VM_VERSION}.vdi"
