#!/bin/bash

# Function to get the current version
# Returns: The current version or falls back to default if not available
_get_version() {
  local version=""

  # Try to read from version file
  if [[ -f "$AM5_VERSION_FILE" && -s "$AM5_VERSION_FILE" ]]; then
    version=$(cat "$AM5_VERSION_FILE")
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
  export AM5_VERSION

  return 0
}

# Images before 5.9.0-alpha.4 still require the secondary HTTP server
# arguments and direct access to the Docker socket.
_uses_legacy_runtime() {
  local version="$1"

  if [[ "$version" =~ ^([0-9]+)\.([0-9]+)\.([0-9]+)(-([[:alnum:].-]+))?$ ]]; then
    local major="${BASH_REMATCH[1]}"
    local minor="${BASH_REMATCH[2]}"
    local patch="${BASH_REMATCH[3]}"
    local prerelease="${BASH_REMATCH[5]:-}"

    if ((major < 5 || (major == 5 && minor < 9))); then
      return 0
    fi

    if ((major == 5 && minor == 9 && patch == 0)) &&
      [[ "$prerelease" =~ ^alpha\.([0-9]+)$ ]]; then
      local alpha="${BASH_REMATCH[1]}"
      if ((alpha < 4)); then
        return 0
      fi
    fi
  fi

  return 1
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
