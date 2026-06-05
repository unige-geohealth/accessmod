#!/bin/bash
set -euo pipefail

# shellcheck source=/dev/null
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/helpers.sh"

convert_script="$(cat "$QEMU_TEST_ROOT_DIR/qemu/scripts/convert_to_ova.sh")"
ovf_template="$(cat "$QEMU_TEST_ROOT_DIR/qemu/templates/vm.ovf.template")"
provision_script="$(cat "$QEMU_TEST_ROOT_DIR/qemu/scripts/provision.sh")"

assert_contains "$convert_script" 'DISPLAY_WIDTH="1024"' "OVA conversion sets readable display width"
assert_contains "$convert_script" 'DISPLAY_HEIGHT="768"' "OVA conversion sets readable display height"
assert_contains "$convert_script" 'DISPLAY_VRAM_SIZE="16"' "OVA conversion allocates enough VRAM"
assert_contains "$ovf_template" 'width="{{DISPLAY_WIDTH}}" height="{{DISPLAY_HEIGHT}}"' "OVF template applies display size"

assert_contains "$provision_script" 'setup_console_display' "provisioning configures console display"
assert_contains "$provision_script" 'consolefont="ter-v24n.psf.gz"' "provisioning uses readable console font"
assert_contains "$provision_script" 'rc-update add consolefont boot' "console font persists after boot"
assert_contains "$provision_script" 'setfont /usr/share/consolefonts/ter-v24n.psf.gz' "console font is applied during provisioning"
