#!/bin/bash
set -euo pipefail

# shellcheck source=/dev/null
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/helpers.sh"

test_begin
trap test_end EXIT

VERSIONS_JSON='{"results":[{"name":"5.9.1"},{"name":"5.9.2-alpha.1"}]}'

write_stub wget <<'EOF'
#!/bin/bash
if [[ "${QEMU_TEST_WGET_FAIL:-0}" == "1" ]]; then
  echo "network unavailable"
  exit 1
fi

printf '%s\n' "$QEMU_TEST_WGET_JSON"
EOF

export QEMU_TEST_WGET_JSON="$VERSIONS_JSON"

# shellcheck source=/dev/null
source "$QEMU_TEST_SCRIPTS_DIR/menu_init.sh"

VERSIONS_CACHE_FILE="$QEMU_TEST_TMP_DIR/versions.json"
MSG_LOG="$QEMU_TEST_TMP_DIR/messages"
_msg() {
  printf '%s\n' "$1" >>"$MSG_LOG"
}
_main() {
  printf 'main\n' >>"$MSG_LOG"
}

echo "not-json" >"$VERSIONS_CACHE_FILE"
assert_equals $'5.9.1\n\n5.9.2-alpha.1' "$(_list_versions all)" "invalid cache is refreshed from Docker Hub"

echo '{"results":[{"name":"5.7.9"}]}' >"$VERSIONS_CACHE_FILE"
_select_version production
assert_file_contains "$MSG_LOG" "No compatible AccessMod versions found." "empty compatible list is reported"

echo "not-json" >"$VERSIONS_CACHE_FILE"
export QEMU_TEST_WGET_FAIL=1
MSG_LOG="$QEMU_TEST_TMP_DIR/messages-fetch-fail"
_select_version all
assert_file_contains "$MSG_LOG" "Could not load versions." "fetch failure is reported"
