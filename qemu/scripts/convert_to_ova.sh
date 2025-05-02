#!/bin/sh
set -e

# Check arguments
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <version>"
    exit 1
fi

VERSION="$1"
BUILD_DIR="_build/vm"
VDI_FILE="alpine-accessmod-${VERSION}-x86_64.vdi"
VMDK_FILE="alpine-accessmod-${VERSION}-x86_64.vmdk"
OVF_FILE="alpine-accessmod-${VERSION}-x86_64.ovf"
OVA_FILE="alpine-accessmod-${VERSION}-x86_64.ova"
TEMPLATE_FILE="templates/vm.ovf.template"

# Check if VDI exists
if [ ! -f "${BUILD_DIR}/${VDI_FILE}" ]; then
    echo "Error: VDI file not found at ${BUILD_DIR}/${VDI_FILE}"
    exit 1
fi

cd "${BUILD_DIR}"

# Convert VDI to VMDK
echo "Converting VDI to VMDK..."
qemu-img convert -f vdi -O vmdk "${VDI_FILE}" "${VMDK_FILE}"

# Get VMDK size (using Linux stat syntax)
VMDK_SIZE=$(stat -c %s "${VMDK_FILE}")

# Generate OVF from template
echo "Generating OVF file..."
sed -e "s/{{VMDK_FILE}}/${VMDK_FILE}/g" \
    -e "s/{{VMDK_SIZE}}/${VMDK_SIZE}/g" \
    "../../${TEMPLATE_FILE}" > "${OVF_FILE}"

# Create OVA (ensuring OVF comes first, using GNU tar's ustar format)
echo "Creating OVA file..."
tar --format ustar -cvf "${OVA_FILE}" "${OVF_FILE}" "${VMDK_FILE}"

echo "Successfully created ${OVA_FILE}"
