#!/bin/bash
set -euo pipefail

# shellcheck source=/dev/null
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/helpers.sh"

packages="$(cat "$QEMU_TEST_ROOT_DIR/qemu/packages")"

for package in bash dialog jq ca-certificates wget docker sudo rsync; do
  assert_contains "$packages" "$package" "qemu packages include $package"
done
