#!/bin/bash

QEMU_TEST_ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
QEMU_TEST_SCRIPTS_DIR="$QEMU_TEST_ROOT_DIR/qemu/fs/home/accessmod/scripts"
QEMU_TEST_TMP_DIR=""
QEMU_TEST_BIN_DIR=""
QEMU_TEST_ORIGINAL_PATH="$PATH"

test_begin() {
  QEMU_TEST_TMP_DIR="$(mktemp -d)"
  QEMU_TEST_BIN_DIR="$QEMU_TEST_TMP_DIR/bin"
  mkdir -p "$QEMU_TEST_BIN_DIR"
  export PATH="$QEMU_TEST_BIN_DIR:$QEMU_TEST_ORIGINAL_PATH"

  export AM5_SCRIPTS_FOLDER="$QEMU_TEST_SCRIPTS_DIR"
  export AM5_HUB_API="https://hub.docker.com/v2/"
  export AM5_REPO="fredmoser/accessmod"
  export AM5_NAME="accessmod"
  export AM5_VERSION="5.9.2-alpha.1"
  export AM5_VERSION_LATEST="latest"
  export AM5_MIN_VERSION="5.8"
  export AM5_VERSION_FILE="$QEMU_TEST_TMP_DIR/version"
  export AM5_PORT_APP="3000"
  export AM5_PORT_APP_PUBLIC="8080"
  export AM5_PORT_HTTP="5000"
  export AM5_PORT_HTTP_PUBLIC="8888"
  export AM5_ARCHIVE_PATH="$QEMU_TEST_TMP_DIR/docker_image.tar.gz"
  export INTERACTIVE=0
  export TMP_FILE="$QEMU_TEST_TMP_DIR/res"
}

test_end() {
  export PATH="$QEMU_TEST_ORIGINAL_PATH"
  if [[ -n "$QEMU_TEST_TMP_DIR" ]]; then
    rm -rf "$QEMU_TEST_TMP_DIR"
  fi
}

write_stub() {
  local name="$1"
  local path="$QEMU_TEST_BIN_DIR/$name"

  cat >"$path"
  chmod +x "$path"
}

assert_equals() {
  local expected="$1"
  local actual="$2"
  local label="$3"

  if [[ "$actual" != "$expected" ]]; then
    printf 'not ok - %s\nexpected:\n%s\nactual:\n%s\n' "$label" "$expected" "$actual" >&2
    exit 1
  fi

  printf 'ok - %s\n' "$label"
}

assert_contains() {
  local haystack="$1"
  local needle="$2"
  local label="$3"

  if [[ "$haystack" != *"$needle"* ]]; then
    printf 'not ok - %s\nmissing: %s\nactual:\n%s\n' "$label" "$needle" "$haystack" >&2
    exit 1
  fi

  printf 'ok - %s\n' "$label"
}

assert_not_contains() {
  local haystack="$1"
  local needle="$2"
  local label="$3"

  if [[ "$haystack" == *"$needle"* ]]; then
    printf 'not ok - %s\nunexpected: %s\nactual:\n%s\n' "$label" "$needle" "$haystack" >&2
    exit 1
  fi

  printf 'ok - %s\n' "$label"
}

assert_file_contains() {
  local file="$1"
  local needle="$2"
  local label="$3"
  local content

  content="$(cat "$file")"
  assert_contains "$content" "$needle" "$label"
}

assert_success() {
  local label="$1"
  shift

  if ! "$@"; then
    printf 'not ok - %s\n' "$label" >&2
    exit 1
  fi

  printf 'ok - %s\n' "$label"
}

assert_failure() {
  local label="$1"
  shift

  if "$@"; then
    printf 'not ok - %s\nexpected command to fail\n' "$label" >&2
    exit 1
  fi

  printf 'ok - %s\n' "$label"
}
