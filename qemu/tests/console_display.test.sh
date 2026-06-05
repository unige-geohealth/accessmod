#!/bin/bash
set -euo pipefail

# shellcheck source=/dev/null
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/helpers.sh"

convert_script="$(cat "$QEMU_TEST_ROOT_DIR/qemu/scripts/convert_to_ova.sh")"
ovf_template="$(cat "$QEMU_TEST_ROOT_DIR/qemu/templates/vm.ovf.template")"
assert_contains "$convert_script" 'DISPLAY_WIDTH="1024"' "OVA conversion sets readable display width"
assert_contains "$convert_script" 'DISPLAY_HEIGHT="768"' "OVA conversion sets readable display height"
assert_contains "$convert_script" 'DISPLAY_VRAM_SIZE="16"' "OVA conversion allocates enough VRAM"
assert_contains "$ovf_template" 'width="{{DISPLAY_WIDTH}}" height="{{DISPLAY_HEIGHT}}"' "OVF template applies display size"
