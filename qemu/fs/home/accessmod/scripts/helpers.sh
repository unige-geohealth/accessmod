#!/bin/bash

# Function to get the current version
# Returns: The current version or falls back to default if not available
_get_version() {
  local version=""

  # Try to read from version file
  if [[ -f "$AM5_VERSION_FILE" && -s "$AM5_VERSION_FILE" ]]; then
    version=$(<"$AM5_VERSION_FILE")
  fi

  # If version is still empty, try environment variable
  if [[ -z "$version" && -n "$AM5_VERSION_ORIG" ]]; then
    version="$AM5_VERSION_ORIG"
  fi

  # Final fallback to latest
  if [[ -z "$version" ]]; then
    version="$AM5_VERSION_LATEST"
  fi

  echo "$version"
}

# Function to set the version
# Args: $1 - The version to set
# Returns: 0 on success, 1 on failure
_set_version() {
  local new_version="$1"

  if [[ -z "$new_version" ]]; then
    echo "Error: Cannot set empty version" >&2
    return 1
  fi

  # Write version to file
  echo "$new_version" >"$AM5_VERSION_FILE"

  # Update in-memory environment variable
  AM5_VERSION="$new_version"

  return 0
}

# Function to check HTTP status of the health endpoint
_check_http_status() {
  local response http_code

  response=$(wget --server-response --spider --timeout=5 "$HEALTH_URL" 2>&1)
  http_code=$(echo "$response" | awk '/^  HTTP/{print $2}' | tail -n 1)

  [[ "$http_code" -eq 200 ]]
}

# Print human-readable HTTP status
_check_http_status_text() {
  if _check_http_status; then
    echo "HTTP 200 OK"
  else
    echo "Failed or Not HTTP 200"
  fi
}

# Wrapper to check if the server is healthy
_check_server_health() {
  _check_http_status
}
