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

assert_equals $'5.9.1\n\n5.8.2\n\nlatest' "$production_versions" "production list keeps only stable supported versions"
assert_equals $'5.10.0-alpha.0\n\n5.9.2-alpha.1\n\n5.9.1\n\n5.9-c\n\n5.8.3-beta.0\n\n5.8.2\n\nlatest' "$all_versions" "all list keeps prereleases and future minors"
