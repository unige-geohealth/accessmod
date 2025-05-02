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
MF_FILE="alpine-accessmod-${VERSION}-x86_64.mf"

# Check if VDI exists
if [ ! -f "${BUILD_DIR}/${VDI_FILE}" ]; then
    echo "Error: VDI file not found at ${BUILD_DIR}/${VDI_FILE}"
    exit 1
fi

cd "${BUILD_DIR}"

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


echo "SHA1(${OVF_FILE})=$(sha1sum ${OVF_FILE} | cut -d' ' -f1)" > $MF_FILE
echo "SHA1(${VMDK_FILE})=$(sha1sum ${VMDK_FILE} | cut -d' ' -f1)" >> $MF_FILE


# Create OVA (ensuring OVF comes first, using GNU tar's ustar format)
echo "Creating OVA file..."
tar --format=ustar -cvf "${OVA_FILE}" "${OVF_FILE}" "${MF_FILE}" "${VMDK_FILE}"

# Clean up intermediate files
echo "Cleaning up..."
rm -f "${VMDK_FILE}" "${OVF_FILE}" "${MF_FILE}" "${VDI_FILE}"

echo "Successfully created ${OVA_FILE}"
