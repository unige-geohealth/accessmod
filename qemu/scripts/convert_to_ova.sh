#!/bin/sh
set -e

# Navigate to the qemu/ root directory
SCRIPT_DIR=$(dirname "$0")
ROOT_DIR=$(cd "$SCRIPT_DIR/.." && pwd)
cd "$ROOT_DIR"

# Load build configuration
. ./config.sh

# Usage: $0 <version> [architecture]
if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
    echo "Usage: $0 <version> [architecture]"
    exit 1
fi

VERSION="$1"
ARCH="${2:-x86_64}"

# Build directory from config (relative to ROOT_DIR)
BUILD_PATH="$BUILD_DIR"

# Construct base names
BASE_NAME="${VM_NAME}-${VERSION}-${ARCH}"
VDI_FILE="${BASE_NAME}.vdi"
VMDK_FILE="${BASE_NAME}.vmdk"
OVF_FILE="${BASE_NAME}.ovf"
OVA_FILE="${BASE_NAME}.ova"
MF_FILE="${BASE_NAME}.mf"
TEMPLATE_FILE="templates/vm.ovf.template"

# Check if VDI exists
if [ ! -f "${BUILD_PATH}/${VDI_FILE}" ]; then
    echo "Error: VDI file not found at ${BUILD_PATH}/${VDI_FILE}"
    exit 1
fi

cd "${BUILD_PATH}"

# Convert VDI to VMDK
echo "Converting VDI to VMDK..."
qemu-img convert -f vdi -O vmdk "${VDI_FILE}" "${VMDK_FILE}"

# Generate UUIDs
DISK_UUID=$(uuidgen)
VM_UUID=$(uuidgen)
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Generate OVF from template
echo "Generating OVF file..."
sed -e "s/{{VMDK_FILE}}/${VMDK_FILE}/g" \
    -e "s/{{DISK_UUID}}/${DISK_UUID}/g" \
    -e "s/{{VM_UUID}}/${VM_UUID}/g" \
    -e "s/{{VERSION}}/${VERSION}/g" \
    -e "s/{{TIMESTAMP}}/${TIMESTAMP}/g" \
    "../../${TEMPLATE_FILE}" > "${OVF_FILE}"

echo "SHA1(${OVF_FILE})=$(sha1sum "${OVF_FILE}" | cut -d' ' -f1)" > "$MF_FILE"
echo "SHA1(${VMDK_FILE})=$(sha1sum "${VMDK_FILE}" | cut -d' ' -f1)" >> "$MF_FILE"

# Create OVA (ensuring OVF comes first, using GNU tar's ustar format)
echo "Creating OVA file..."
tar --format=ustar -cvf "${OVA_FILE}" "${OVF_FILE}" "${MF_FILE}" "${VMDK_FILE}"

# Clean up intermediate files
echo "Cleaning up..."
rm -f "${VMDK_FILE}" "${OVF_FILE}" "${MF_FILE}" "${VDI_FILE}"

echo "Successfully created ${OVA_FILE}"
