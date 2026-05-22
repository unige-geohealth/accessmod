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

case "$ARCH" in
    x86_64)
        OVF_OS_ID="102"
        OS_DESCRIPTION="Other_64"
        VBOX_OS_TYPE="Linux_64"
        CHIPSET_TYPE="ICH9"
        GRAPHICS_CONTROLLER="VMSVGA"
        ;;
    aarch64)
        OVF_OS_ID="110"
        OS_DESCRIPTION="Linux ARM 64"
        VBOX_OS_TYPE="Linux_arm64"
        CHIPSET_TYPE="ARMv8Virtual"
        GRAPHICS_CONTROLLER="QemuRamFB"
        ;;
    *)
        echo "Error: unsupported architecture: $ARCH" >&2
        exit 1
        ;;
esac

# Check if VDI exists
if [ ! -f "${BUILD_PATH}/${VDI_FILE}" ]; then
    echo "Error: VDI file not found at ${BUILD_PATH}/${VDI_FILE}"
    exit 1
fi

cd "${BUILD_PATH}"

# Convert VDI to VMDK
echo "Converting VDI to VMDK..."
qemu-img convert -c -f vdi -O vmdk -o subformat=streamOptimized "${VDI_FILE}" "${VMDK_FILE}"


# Generate UUIDs
DISK_UUID=$(uuidgen)
VM_UUID=$(uuidgen)
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

sha1_file() {
    if command -v sha1sum >/dev/null 2>&1; then
        sha1sum "$1" | cut -d' ' -f1
    else
        shasum -a 1 "$1" | cut -d' ' -f1
    fi
}

print_platform_section() {
    if [ "$ARCH" = "aarch64" ]; then
        cat <<'EOF'
      <Platform architecture="ARM">
        <Chipset type="ARMv8Virtual"/>
        <CPU/>
      </Platform>
EOF
    fi
}

print_cpu_section() {
    if [ "$ARCH" = "aarch64" ]; then
        cat <<'EOF'
        <CPU count="2"/>
EOF
    else
        cat <<'EOF'
        <CPU count="2">
          <PAE enabled="true"/>
          <LongMode enabled="true"/>
          <X2APIC enabled="true"/>
          <HardwareVirtExLargePages enabled="true"/>
        </CPU>
EOF
    fi
}

# Generate OVF from template
echo "Generating OVF file..."
while IFS= read -r line; do
    case "$line" in
        *"{{PLATFORM_SECTION}}"*)
            print_platform_section
            ;;
        *"{{CPU_SECTION}}"*)
            print_cpu_section
            ;;
        *)
            printf '%s\n' "$line" | sed -e "s/{{VMDK_FILE}}/${VMDK_FILE}/g" \
                -e "s/{{DISK_UUID}}/${DISK_UUID}/g" \
                -e "s/{{VM_UUID}}/${VM_UUID}/g" \
                -e "s/{{VERSION}}/${VERSION}/g" \
                -e "s/{{OVF_OS_ID}}/${OVF_OS_ID}/g" \
                -e "s/{{OS_DESCRIPTION}}/${OS_DESCRIPTION}/g" \
                -e "s/{{VBOX_OS_TYPE}}/${VBOX_OS_TYPE}/g" \
                -e "s/{{CHIPSET_TYPE}}/${CHIPSET_TYPE}/g" \
                -e "s/{{GRAPHICS_CONTROLLER}}/${GRAPHICS_CONTROLLER}/g" \
                -e "s/{{TIMESTAMP}}/${TIMESTAMP}/g"
            ;;
    esac
done < "../../${TEMPLATE_FILE}" > "${OVF_FILE}"

echo "SHA1(${OVF_FILE})=$(sha1_file "${OVF_FILE}")" > "$MF_FILE"
echo "SHA1(${VMDK_FILE})=$(sha1_file "${VMDK_FILE}")" >> "$MF_FILE"

# Create OVA (ensuring OVF comes first, using GNU tar's ustar format with XZ compression)
echo "Creating OVA file"
tar --format=ustar -cvf "${OVA_FILE}" "${OVF_FILE}" "${MF_FILE}" "${VMDK_FILE}"

# Clean up intermediate files
echo "Cleaning up..."
rm -f "${VMDK_FILE}" "${OVF_FILE}" "${MF_FILE}" "${VDI_FILE}"

echo "Successfully created ${OVA_FILE}"
