#!/bin/bash

_check_http_status() {
  response=$(wget --server-response --spider --timeout=5 "$HEALTH_URL" 2>&1)
  http_code=$(echo "$response" | awk '/^  HTTP/{print $2}' | tail -1)

  if [ "$http_code" -eq 200 ]; then
    return 0  # HTTP 200 OK
  else
    return 1  # Not HTTP 200
  fi
}

_check_http_status_text() {
  if _check_http_status; then
    echo "HTTP 200 OK"
  else
    echo "Failed or Not HTTP 200"
  fi
}
