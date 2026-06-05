#!/bin/bash
set -euo pipefail

# shellcheck source=/dev/null
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/helpers.sh"

test_begin
trap test_end EXIT

DOCKER_LOG="$QEMU_TEST_TMP_DIR/docker.log"
DOCKER_STATE="$QEMU_TEST_TMP_DIR/docker-state"
mkdir -p "$DOCKER_STATE"
export DOCKER_LOG DOCKER_STATE

write_stub docker <<'EOF'
#!/bin/bash
printf 'docker' >>"$DOCKER_LOG"
for arg in "$@"; do
  printf ' %s' "$arg" >>"$DOCKER_LOG"
done
printf '\n' >>"$DOCKER_LOG"

case "$1" in
  images)
    if [[ "${2:-}" == "-q" ]]; then
      image="${3:-}"
      case "${DOCKER_SCENARIO:-}" in
        local)
          [[ "$image" == "$AM5_REPO:$DOCKER_SELECTED_VERSION" ]] && echo "image-id"
          ;;
        archive_load)
          [[ -f "$DOCKER_STATE/loaded" && "$image" == "$AM5_REPO:$DOCKER_SELECTED_VERSION" ]] && echo "image-id"
          ;;
      esac
      exit 0
    fi

    if [[ "${DOCKER_SCENARIO:-}" == "archive_available" && "${2:-}" == "$AM5_REPO" ]]; then
      echo "${DOCKER_AVAILABLE_VERSION:-5.9.0}"
      exit 0
    fi
    ;;
  load)
    touch "$DOCKER_STATE/loaded"
    exit 0
    ;;
  pull)
    case "${DOCKER_SCENARIO:-}" in
      remote_success)
        [[ "${2:-}" == "$AM5_REPO:$DOCKER_SELECTED_VERSION" ]] && exit 0
        exit 1
        ;;
      remote_latest)
        [[ "${2:-}" == "$AM5_REPO:latest" ]] && exit 0
        exit 1
        ;;
      *)
        exit 1
        ;;
    esac
    ;;
  run)
    if [[ "${2:-}" == "--rm" ]]; then
      echo "${DOCKER_LATEST_VERSION:-5.9.1}"
    fi
    exit 0
    ;;
  ps)
    exit 0
    ;;
  stop|rm)
    exit 0
    ;;
esac

exit 0
EOF

# shellcheck source=/dev/null
source "$QEMU_TEST_SCRIPTS_DIR/start.sh"
_msg() {
  :
}

DOCKER_SCENARIO=local
DOCKER_SELECTED_VERSION="5.9.2-alpha.1"
export DOCKER_SCENARIO DOCKER_SELECTED_VERSION
>"$DOCKER_LOG"
_ensure_docker_image "$DOCKER_SELECTED_VERSION"
log="$(cat "$DOCKER_LOG")"
assert_contains "$log" "docker images -q fredmoser/accessmod:5.9.2-alpha.1" "existing image is checked locally"
assert_not_contains "$log" "docker pull" "existing local image skips pull"

touch "$AM5_ARCHIVE_PATH"
DOCKER_SCENARIO=archive_load
>"$DOCKER_LOG"
rm -f "$DOCKER_STATE/loaded"
_ensure_docker_image "$DOCKER_SELECTED_VERSION"
log="$(cat "$DOCKER_LOG")"
assert_contains "$log" "docker load -i $AM5_ARCHIVE_PATH" "missing image loads bundled archive"

DOCKER_SCENARIO=archive_available
DOCKER_AVAILABLE_VERSION="5.9.0"
export DOCKER_AVAILABLE_VERSION
>"$DOCKER_LOG"
_ensure_docker_image "$DOCKER_SELECTED_VERSION"
assert_equals "5.9.0" "$(cat "$AM5_VERSION_FILE")" "archive fallback stores available image version"

rm -f "$AM5_ARCHIVE_PATH"
DOCKER_SCENARIO=remote_latest
DOCKER_LATEST_VERSION="5.9.1"
export DOCKER_LATEST_VERSION
>"$DOCKER_LOG"
_ensure_docker_image "$DOCKER_SELECTED_VERSION"
log="$(cat "$DOCKER_LOG")"
assert_contains "$log" "docker pull fredmoser/accessmod:5.9.2-alpha.1" "remote fallback tries selected image"
assert_contains "$log" "docker pull fredmoser/accessmod:latest" "remote fallback tries latest image"
assert_equals "5.9.1" "$(cat "$AM5_VERSION_FILE")" "remote latest fallback stores image version"

>"$DOCKER_LOG"
_start_container "5.9.2-alpha.1"
log="$(cat "$DOCKER_LOG")"
assert_contains "$log" "docker run --name accessmod" "docker run sets container name"
assert_contains "$log" "--health-cmd=wget --spider http://localhost:3000/" "docker run sets healthcheck"
assert_contains "$log" "-p 3000:3000" "docker run exposes app port"
assert_contains "$log" "-p 5000:5000" "docker run exposes http port"
assert_contains "$log" "-v /var/run/docker.sock:/var/run/docker.sock" "docker run mounts docker socket"
assert_contains "$log" "-v am_data_grass:/data/dbgrass" "docker run mounts GRASS data volume"
assert_contains "$log" "--restart unless-stopped" "docker run sets restart policy"
assert_contains "$log" "fredmoser/accessmod:5.9.2-alpha.1" "docker run uses selected image"
assert_contains "$log" "Rscript --vanilla run.r 3000 5000 8888" "docker run starts AccessMod directly"
