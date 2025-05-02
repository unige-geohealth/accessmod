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

# Create temporary directories and copy scripts
TEMP_DIR="/tmp/scripts"
mkdir -p "$TEMP_DIR/setup"

# Copy all scripts
cp -r scripts/* "$TEMP_DIR/"
cp scripts/setup/* "$TEMP_DIR/setup/"
chmod +x "$TEMP_DIR"/*.sh "$TEMP_DIR/setup"/*.sh

# Copy config for setup scripts
cp config.sh /tmp/

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
    # Run setup scripts in order
    echo "Starting AccessMod VM setup..."
    for script in /tmp/scripts/setup/[0-9][0-9]_*.sh; do
        echo "Running setup script: \$(basename \$script)..."
        sh \$script
    done

EOF

echo "VM image created successfully at ${BUILD_DIR}/${VM_NAME}-${VM_VERSION}.vdi"

# Clean up temporary directories
rm -rf "$TEMP_DIR" /tmp/config.sh
