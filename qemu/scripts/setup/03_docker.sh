#!/bin/sh

# Source common functions and variables
. /tmp/scripts/setup/common.sh

log "Starting Docker setup..."

# Check required variables
check_required_vars "AM5_REPO" "AM5_VERSION_LATEST" "AM5_VERSION_FILE"

# Configure and start services
log "Configuring Docker service..."
rc-update add docker boot
rc-update add local default

log "Starting Docker service..."
service docker start
sleep 10  # Wait for Docker to be ready

# Create Docker volumes
log "Creating Docker volumes..."
docker volume create am_data_cache
docker volume create am_data_logs
docker volume create am_data_grass

# Pull base image and get version
log "Pulling base image ${AM5_REPO}:${AM5_VERSION_LATEST}..."
docker pull ${AM5_REPO}:${AM5_VERSION_LATEST}

log "Getting AccessMod version..."
AM5_VERSION=$(docker run --rm ${AM5_REPO}:${AM5_VERSION_LATEST} cat version.txt)

# Save version
log "Saving version information..."
echo ${AM5_VERSION} > ${AM5_VERSION_FILE}
chown ${USERNAME} ${AM5_VERSION_FILE}

# Export version for other scripts
export AM5_VERSION

log "Docker setup completed"
