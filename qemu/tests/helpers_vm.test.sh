#!/bin/bash
set -euo pipefail

# shellcheck source=/dev/null
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/helpers.sh"

test_begin
trap test_end EXIT

write_stub wget <<'EOF'
#!/bin/bash
if [[ "${QEMU_TEST_WGET_STATUS:-200}" == "200" ]]; then
  printf '  HTTP/1.1 200 OK\n' >&2
  exit 0
fi

printf '  HTTP/1.1 503 Service Unavailable\n' >&2
exit 1
EOF

# shellcheck source=/dev/null
source "$QEMU_TEST_SCRIPTS_DIR/env.sh"
# shellcheck source=/dev/null
source "$QEMU_TEST_SCRIPTS_DIR/helpers.sh"

echo "5.9.1" >"$AM5_VERSION_FILE"
assert_equals "5.9.1" "$(_get_version)" "_get_version prefers version file"

rm -f "$AM5_VERSION_FILE"
assert_equals "5.9.2-alpha.1" "$(_get_version)" "_get_version falls back to original environment version"

AM5_VERSION_ORIG=""
AM5_VERSION_LATEST="latest"
assert_equals "latest" "$(_get_version)" "_get_version falls back to latest"

_set_version "5.9.3"
assert_equals "5.9.3" "$(cat "$AM5_VERSION_FILE")" "_set_version writes version file"
assert_equals "5.9.3" "$AM5_VERSION" "_set_version exports in-memory version"
reject_empty_version() {
  _set_version "" 2>/dev/null
}
assert_failure "_set_version rejects empty versions" reject_empty_version

export QEMU_TEST_WGET_STATUS=200
assert_success "_check_http_status accepts HTTP 200" _check_http_status

export QEMU_TEST_WGET_STATUS=503
assert_failure "_check_http_status rejects non-200 status" _check_http_status
