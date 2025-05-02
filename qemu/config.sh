#!/bin/sh

# System configuration
export CPUS="2"
export DISK_SIZE="40960"
export MEMORY="4096"

# Alpine configuration
export ALPINE_VERSION="3.16"
export ALPINE_RELEASE="3.16.8"
export ISO_CHECKSUM="160c76f65ef888cb4cb4758a3b9d81d44f597ce22631aa94ae164c6a529d7b43"
export ALPINE_REPO="https://dl-cdn.alpinelinux.org/alpine/v3.16/community"

# Docker Hub configuration
export HUB_API="https://hub.docker.com/v2/"
export MIN_VERSION="5.8"
export VM_VERSION="5.8"
export REPO="fredmoser/accessmod"
export VERSION_LATEST="latest"

# VM configuration
export VM_NAME="alpine-accessmod"
export VM_DESCRIPTION="AccessMod QEMU VM"

# Network ports
export PORT_APP="3000"
export PORT_APP_PUBLIC="8080"
export PORT_HTTP="5000"
export PORT_HTTP_PUBLIC="8888"
export PORT_SSH="22"
export PORT_SSH_PUBLIC="2222"

# User configuration
export SSH_USERNAME="root"
export SSH_PASSWORD="L3b5SqNDIoarhn5CJJUJvA=="
export USERNAME="accessmod"
export USERPASSWORD="accessmod"

# Build paths
export BUILD_DIR="_build/vm"
export SCRIPTS_DIR="/tmp/scripts"

# Environment variables for provisioning
export AM5_PORT_APP="${PORT_APP}"
export AM5_PORT_APP_PUBLIC="${PORT_APP_PUBLIC}"
export AM5_PORT_HTTP="${PORT_HTTP}"
export AM5_PORT_HTTP_PUBLIC="${PORT_HTTP_PUBLIC}"
export AM5_SCRIPTS_FOLDER="/home/accessmod/scripts"
export AM5_VERSION_FILE="/home/accessmod/version"
export AM5_VERSION_LATEST="${VERSION_LATEST}"
export AM5_MIN_VERSION="${MIN_VERSION}"
export AM5_REPO="${REPO}"
export AM5_HUB_API="${HUB_API}"
