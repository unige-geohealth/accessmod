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
IMAGE_ARCHIVE="fs/home/accessmod/docker_image.tar"
VDI_PATH="${BUILD_DIR}/${VM_NAME}-${VM_VERSION}-${ARCH}.vdi"
OVA_PATH="${BUILD_DIR}/${VM_NAME}-${VM_VERSION}-${ARCH}.ova"

docker pull "${AM5_REPO}:${AM5_VERSION_LATEST}"

AM5_VERSION="$(docker run --rm --entrypoint="" "${AM5_REPO}:${AM5_VERSION_LATEST}" cat version.txt | tr -d '\r\n')"

export AM5_VERSION

echo "Using AM5 image version: ${AM5_VERSION}"

docker save "${AM5_REPO}:${AM5_VERSION_LATEST}" -o $IMAGE_ARCHIVE


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
