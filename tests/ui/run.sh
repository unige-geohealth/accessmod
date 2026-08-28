#!/bin/sh

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/../.." && pwd)
COMPOSE_FILE="$SCRIPT_DIR/docker-compose.yml"
PROJECT_NAME="accessmod_ui_test"
OUTPUT_DIR="$REPO_DIR/tests/_output/ui"
LIVE=0

if [ "${1:-}" = "--live" ]; then
  LIVE=1
  if [ -z "${MAPTILER_API_KEY:-}" ]; then
    echo "MAPTILER_API_KEY is required for npm run test:ui:live" >&2
    exit 2
  fi
fi

mkdir -p "$OUTPUT_DIR"

compose() {
  docker compose --project-name "$PROJECT_NAME" --file "$COMPOSE_FILE" "$@"
}

cleanup() {
  compose logs --no-color shiny > "$OUTPUT_DIR/shiny.log" 2>&1 || true
  compose down --volumes --remove-orphans >/dev/null 2>&1 || true
}

trap cleanup EXIT INT TERM

compose down --volumes --remove-orphans >/dev/null 2>&1 || true

export UI_TEST_LIVE_MAPTILER=$LIVE
compose run --rm init-data
compose up --build --abort-on-container-exit --exit-code-from playwright playwright
