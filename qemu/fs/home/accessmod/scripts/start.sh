#!/bin/bash

# Source required scripts
. $AM5_SCRIPTS_FOLDER/env.sh
. $AM5_SCRIPTS_FOLDER/message.sh 
. $AM5_SCRIPTS_FOLDER/helpers.sh 
. $AM5_SCRIPTS_FOLDER/docker.sh 

# Initialize version using the getter function
AM5_VERSION=$(_get_version)
START_TITLE="Starting AccessMod $AM5_VERSION"

# Function to ensure the Docker image is available
# This handles loading from archive or pulling from remote
_ensure_docker_image() {
    local version="$1"
    local image_tag="$AM5_REPO:$version"
    
    # Check if the specific image version exists
    if docker images -q "$image_tag" > /dev/null 2>&1; then
        _msg "Image $image_tag is already available" --title "$START_TITLE"
        return 0
    fi
    
    # Image doesn't exist - try to load from archive
    if [ -f "$AM5_ARCHIVE_PATH" ]; then
        _msg "Loading image from archive..." --title "$START_TITLE"
        docker load -i "$AM5_ARCHIVE_PATH"
        
        # Check if our version is now available
        if docker images -q "$image_tag" > /dev/null 2>&1; then
            _msg "Successfully loaded $image_tag from archive" --title "$START_TITLE"
            return 0
        fi
        
        # Archive didn't contain our version - check if any version was loaded
        local available_version=$(docker images "$AM5_REPO" --format "{{.Tag}}" | head -1)
        if [ -n "$available_version" ]; then
            _msg "Version $version not found in archive. Using available version $available_version instead" --title "$START_TITLE"
            _set_version "$available_version"
            return 0
        fi
    fi
    
    # No archive or archive didn't work - pull from remote
    _msg "Pulling image $image_tag from remote..." --title "$START_TITLE"
    if docker pull "$image_tag"; then
        _msg "Successfully pulled $image_tag" --title "$START_TITLE"
        return 0
    else
        # Pull failed - try latest as last resort
        _msg "Failed to pull $image_tag, trying latest..." --title "$START_TITLE"
        if docker pull "$AM5_REPO:latest"; then
            local latest_version=$(docker run --rm --entrypoint="" "$AM5_REPO:latest" cat version.txt 2>/dev/null | tr -d '\r\n')
            if [ -n "$latest_version" ]; then
                _set_version "$latest_version"
            else
                _set_version "latest"
            fi
            return 0
        fi
    fi
    
    _msg "Failed to obtain any usable image" --title "$START_TITLE"
    return 1
}

# Function to stop and remove any running container
_stop_container() {
    local running=$(docker ps -qa --filter name=$AM5_NAME)
    if [ -n "$running" ]; then
        _msg "Stopping and removing existing container $AM5_NAME" --title "$START_TITLE"
        docker stop $running
        docker rm $running
    fi
}

# Function to start the container
_start_container() {
    local version="$1"
    local image_tag="$AM5_REPO:$version"
    
    _msg "Starting container $AM5_NAME with image $image_tag" --title "$START_TITLE"
    
    docker run \
        --name $AM5_NAME \
        --health-cmd="wget --spider $HEALTH_URL" \
        --health-interval=1m \
        --health-retries=10 \
        --health-start-period=10s \
        -p $AM5_PORT_APP:$AM5_PORT_APP \
        -p $AM5_PORT_HTTP:$AM5_PORT_HTTP \
        -v /var/run/docker.sock:/var/run/docker.sock \
        -v /tmp:/tmp \
        -v am_data_logs:/data/logs \
        -v am_data_cache:/data/cache \
        -v am_data_grass:/data/dbgrass \
        -d \
        --restart unless-stopped \
        $image_tag \
        Rscript \
        --vanilla \
        run.r \
        $AM5_PORT_APP \
        $AM5_PORT_HTTP \
        $AM5_PORT_HTTP_PUBLIC
    
    _msg "Container $AM5_NAME is starting (this could take a minute)" \
        --duration 10 \
        --title "$START_TITLE"
}

# Main execution flow
main() {
    _msg "Start requested..." --duration 2 --title "$START_TITLE"
    
    # Check if system is ready
    if [ ! -e $AM5_SCRIPTS_FOLDER/ready ]; then
        _msg "System not ready" --duration 2 --title "$START_TITLE"
        return 1
    fi
    
    # Ensure Docker image is available
    if ! _ensure_docker_image "$AM5_VERSION"; then
        _msg "Failed to obtain Docker image" --duration 5 --title "$START_TITLE"
        return 1
    fi
    
    # Stop any running container
    _stop_container
    
    # Start the container with the current version
    _start_container "$AM5_VERSION"
    
    return 0
}

# Run the main function
main
