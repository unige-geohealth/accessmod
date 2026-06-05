#!/bin/bash
set -euo pipefail

# shellcheck source=/dev/null
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/helpers.sh"

test_begin
trap test_end EXIT

# shellcheck source=/dev/null
source "$QEMU_TEST_SCRIPTS_DIR/menu_init.sh"

VERSIONS_CACHE_FILE="$QEMU_TEST_TMP_DIR/versions.json"

cat >"$VERSIONS_CACHE_FILE" <<'JSON'
{
  "results": [
    { "name": "5.10.0-alpha.0" },
    { "name": "5.9.2-alpha.1" },
    { "name": "5.9.1" },
    { "name": "5.9-c" },
    { "name": "5.8.3-beta.0" },
    { "name": "5.8.2" },
    { "name": "5.7.9" },
    { "name": "latest" },
    { "name": "not-a-version" }
  ]
}
JSON

production_versions="$(_list_versions production)"
all_versions="$(_list_versions all)"

assert_equals $'5.9.1\n5.8.2\nlatest' "$production_versions" "production list keeps only stable supported versions"
assert_equals $'5.10.0-alpha.0\n5.9.2-alpha.1\n5.9.1\n5.9-c\n5.8.3-beta.0\n5.8.2\nlatest' "$all_versions" "all list keeps prereleases and future minors"

export QEMU_TEST_DIALOG_LOG="$QEMU_TEST_TMP_DIR/dialog.log"
export QEMU_TEST_DIALOG_SELECTION="5.9.1"
export QEMU_TEST_UPDATED_VERSION="$QEMU_TEST_TMP_DIR/updated-version"

write_stub dialog <<'EOF'
#!/bin/bash
printf 'args:' >>"$QEMU_TEST_DIALOG_LOG"
for arg in "$@"; do
  printf '<%s>' "$arg" >>"$QEMU_TEST_DIALOG_LOG"
done
printf '\n' >>"$QEMU_TEST_DIALOG_LOG"
printf '%s' "$QEMU_TEST_DIALOG_SELECTION" >&2
EOF

_update() {
  printf '%s\n' "$1" >"$QEMU_TEST_UPDATED_VERSION"
}

_select_version production

dialog_log="$(cat "$QEMU_TEST_DIALOG_LOG")"
assert_contains "$dialog_log" '<5.9.1><><5.8.2><><latest><>' "production menu passes tag/description pairs"
assert_equals "5.9.1" "$(cat "$QEMU_TEST_UPDATED_VERSION")" "production selection is passed to updater"

export QEMU_TEST_DIALOG_SELECTION="5.10.0-alpha.0"
>"$QEMU_TEST_DIALOG_LOG"
>"$QEMU_TEST_UPDATED_VERSION"

_select_version all

dialog_log="$(cat "$QEMU_TEST_DIALOG_LOG")"
assert_contains "$dialog_log" '<5.10.0-alpha.0><><5.9.2-alpha.1><><5.9.1><><5.9-c><><5.8.3-beta.0><><5.8.2><><latest><>' "all menu passes tag/description pairs"
assert_equals "5.10.0-alpha.0" "$(cat "$QEMU_TEST_UPDATED_VERSION")" "all selection is passed to updater"
