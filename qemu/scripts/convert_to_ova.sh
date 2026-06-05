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
        MACHINE_SETTINGS_VERSION="1.19-linux"
        OVF_OS_ID="102"
        OS_DESCRIPTION="Other_64"
        VBOX_OS_TYPE="Linux_64"
        CHIPSET_TYPE="ICH9"
        GRAPHICS_CONTROLLER="VMSVGA"
        ;;
    aarch64)
        MACHINE_SETTINGS_VERSION="1.20-linux"
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
        <RTC localOrUTC="UTC"/>
        <Chipset type="ARMv8Virtual"/>
        <CPU count="2"/>
        <arm>
          <CPU/>
        </arm>
      </Platform>
EOF
    fi
}

print_hardware_cpu_section() {
    if [ "$ARCH" = "aarch64" ]; then
        return
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

print_hardware_chipset_section() {
    if [ "$ARCH" = "aarch64" ]; then
        return
    else
        cat <<EOF
        <Chipset type="${CHIPSET_TYPE}"/>
EOF
    fi
}

print_hardware_hid_section() {
    if [ "$ARCH" = "aarch64" ]; then
        cat <<'EOF'
        <HID Pointing="USBTablet" Keyboard="USBKeyboard"/>
EOF
    fi
}

print_hardware_usb_section() {
    if [ "$ARCH" = "aarch64" ]; then
        cat <<'EOF'
        <USB>
          <Controllers>
            <Controller name="OHCI" type="OHCI"/>
            <Controller name="xHCI" type="XHCI"/>
          </Controllers>
        </USB>
EOF
    fi
}

print_hardware_rtc_section() {
    if [ "$ARCH" = "aarch64" ]; then
        return
    else
        cat <<'EOF'
        <RTC localOrUTC="UTC"/>
EOF
    fi
}

print_ovf_storage_controller_section() {
    if [ "$ARCH" = "aarch64" ]; then
        cat <<'EOF'
      <Item>
        <rasd:Address>0</rasd:Address>
        <rasd:Caption>virtioSCSIController0</rasd:Caption>
        <rasd:Description>Virtio-SCSI Controller</rasd:Description>
        <rasd:ElementName>virtioSCSIController0</rasd:ElementName>
        <rasd:InstanceID>3</rasd:InstanceID>
        <rasd:ResourceSubType>virtio-scsi</rasd:ResourceSubType>
        <rasd:ResourceType>6</rasd:ResourceType>
      </Item>
EOF
    else
        cat <<'EOF'
      <Item>
        <rasd:Address>0</rasd:Address>
        <rasd:Caption>ideController0</rasd:Caption>
        <rasd:Description>IDE Controller</rasd:Description>
        <rasd:ElementName>ideController0</rasd:ElementName>
        <rasd:InstanceID>3</rasd:InstanceID>
        <rasd:ResourceSubType>PIIX4</rasd:ResourceSubType>
        <rasd:ResourceType>5</rasd:ResourceType>
      </Item>
      <Item>
        <rasd:Address>1</rasd:Address>
        <rasd:Caption>ideController1</rasd:Caption>
        <rasd:Description>IDE Controller</rasd:Description>
        <rasd:ElementName>ideController1</rasd:ElementName>
        <rasd:InstanceID>4</rasd:InstanceID>
        <rasd:ResourceSubType>PIIX4</rasd:ResourceSubType>
        <rasd:ResourceType>5</rasd:ResourceType>
      </Item>
EOF
    fi
}

print_vbox_storage_controllers_section() {
    if [ "$ARCH" = "aarch64" ]; then
        cat <<EOF
        <StorageControllers>
          <StorageController name="VirtioSCSI Controller" type="VirtioSCSI" PortCount="16" useHostIOCache="false" Bootable="true">
            <AttachedDevice type="HardDisk" hotpluggable="false" port="0" device="0">
              <Image uuid="${DISK_UUID}"/>
            </AttachedDevice>
          </StorageController>
        </StorageControllers>
EOF
    else
        cat <<EOF
        <StorageControllers>
          <StorageController name="IDE Controller" type="PIIX4" PortCount="2" useHostIOCache="true" Bootable="true">
            <AttachedDevice type="HardDisk" hotpluggable="false" port="0" device="0">
              <Image uuid="${DISK_UUID}"/>
            </AttachedDevice>
          </StorageController>
        </StorageControllers>
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
        *"{{HARDWARE_CPU_SECTION}}"*)
            print_hardware_cpu_section
            ;;
        *"{{HARDWARE_CHIPSET_SECTION}}"*)
            print_hardware_chipset_section
            ;;
        *"{{HARDWARE_HID_SECTION}}"*)
            print_hardware_hid_section
            ;;
        *"{{HARDWARE_USB_SECTION}}"*)
            print_hardware_usb_section
            ;;
        *"{{HARDWARE_RTC_SECTION}}"*)
            print_hardware_rtc_section
            ;;
        *"{{OVF_STORAGE_CONTROLLER_SECTION}}"*)
            print_ovf_storage_controller_section
            ;;
        *"{{VBOX_STORAGE_CONTROLLERS_SECTION}}"*)
            print_vbox_storage_controllers_section
            ;;
        *)
            printf '%s\n' "$line" | sed -e "s/{{VMDK_FILE}}/${VMDK_FILE}/g" \
                -e "s/{{DISK_UUID}}/${DISK_UUID}/g" \
                -e "s/{{VM_UUID}}/${VM_UUID}/g" \
                -e "s/{{VERSION}}/${VERSION}/g" \
                -e "s/{{OVF_OS_ID}}/${OVF_OS_ID}/g" \
                -e "s/{{OS_DESCRIPTION}}/${OS_DESCRIPTION}/g" \
                -e "s/{{VBOX_OS_TYPE}}/${VBOX_OS_TYPE}/g" \
                -e "s/{{MACHINE_SETTINGS_VERSION}}/${MACHINE_SETTINGS_VERSION}/g" \
                -e "s/{{CHIPSET_TYPE}}/${CHIPSET_TYPE}/g" \
                -e "s/{{GRAPHICS_CONTROLLER}}/${GRAPHICS_CONTROLLER}/g" \
                -e "s/{{OVF_DISK_PARENT}}/3/g" \
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
