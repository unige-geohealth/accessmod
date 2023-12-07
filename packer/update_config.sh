#!/bin/bash
set -e

# Function to check required commands
check_command() {
  if ! command -v "$1" &> /dev/null; then
    echo "Missing command $1"
    exit 1
  fi
}

# Check for necessary commands
check_command 'jq'
check_command 'packer'
check_command 'docker'

# Configuration variables
VM_VERSION="5.8"
AM5_IMAGE="fredmoser/accessmod"
TAG_CI="ci"
PACKER_CONF="./alpine.json" 
ISO_URL="https://dl-cdn.alpinelinux.org/alpine/v3.14/releases/x86_64/alpine-virt-3.14.8-x86_64.iso"
ISO_LOCAL="iso/alpine-virt-3.14.8-x86_64.iso"
ISO_CHECKSUM="2f46f2340ba82763e10984a56dd4bd9002f8cd12f45d617bb81a7597d2033250"

# Pulling Docker image and getting version
docker pull "$AM5_IMAGE:$TAG_CI"
AM5_VERSION=$(docker run --rm "$AM5_IMAGE:$TAG_CI" cat version.txt)

if [[ -z $AM5_VERSION ]]; then
  echo "no version"
  exit 1
fi

if [[ ! -e $PACKER_CONF ]]; then
  echo "no packer conf $PACKER_CONF"
  exit 1
fi

# Updating Packer configuration
jq --arg vm_version "$VM_VERSION" \
   --arg version "$AM5_VERSION" \
   --arg image "$AM5_IMAGE:$AM5_VERSION" \
   --arg iso_url "$ISO_URL" \
   --arg iso_local "$ISO_LOCAL" \
   --arg iso_checksum "$ISO_CHECKSUM" \
   '.variables.vm_version = $vm_version |
    .variables.version = $version |
    .variables.image = $image |
    .variables.iso_download_url = $iso_url |
    .variables.iso_local_url = $iso_local |
    .variables.iso_checksum = $iso_checksum' "$PACKER_CONF" > /tmp/packer.json && mv /tmp/packer.json "$PACKER_CONF"

