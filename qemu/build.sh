#!/bin/sh
set -e

# Source configuration
. ./config.sh

# Parse arguments
# ARCH: target architecture (e.g., x86_64, aarch64)
ARCH=${1:-x86_64}
# VM version override: if supplied as second argument, override default VM_VERSION
if [ $# -ge 2 ]; then
    VM_VERSION="$2"
fi

# Create build directory
mkdir -p "$BUILD_DIR"

# Load base packages
PACKAGES=$(cat packages)

IMAGE_ARCHIVE="fs${AM5_ARCHIVE_PATH}"
VDI_PATH="${BUILD_DIR}/${VM_NAME}-${VM_VERSION}-${ARCH}.vdi"
OVA_PATH="${BUILD_DIR}/${VM_NAME}-${VM_VERSION}-${ARCH}.ova"

# Verify that the Docker image archive prepared by the workflow exists
if [ ! -f "$IMAGE_ARCHIVE" ]; then
    echo "Error: Docker image archive not found at $IMAGE_ARCHIVE" >&2
else
    echo "Found Docker image archive at $IMAGE_ARCHIVE"
fi


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
    "$VDI_PATH" -- ./scripts/provision.sh 

# VM image creation complete; report VDI path
echo "VM image created successfully at ${VDI_PATH}"

# OVA conversion (only for x86_64 arch)
echo "Converting VDI to OVA..."
./scripts/convert_to_ova.sh "$VM_VERSION"
echo "OVA created successfully at ${OVA_PATH}"
