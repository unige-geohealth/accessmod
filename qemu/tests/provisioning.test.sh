#!/bin/bash
set -euo pipefail

# shellcheck source=/dev/null
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/helpers.sh"

provision_script="$(cat "$QEMU_TEST_ROOT_DIR/qemu/scripts/provision.sh")"

assert_contains "$provision_script" "setup_dns" "provisioning configures DNS"
assert_contains "$provision_script" "nameserver 1.1.1.1" "provisioning writes Cloudflare DNS"
assert_contains "$provision_script" "nameserver 8.8.8.8" "provisioning writes Google DNS fallback"
assert_contains "$provision_script" "/etc/local.d/accessmod_dns.start" "DNS fix is reapplied at boot"
assert_contains "$provision_script" 'RESOLV_CONF="no"' "DHCP resolver overwrites are disabled when supported"
