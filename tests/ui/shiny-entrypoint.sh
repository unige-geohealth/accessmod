#!/bin/sh

set -eu

TEST_TIMEZONE=${TZ:-UTC}
printf '%s\n' "$TEST_TIMEZONE" > /etc/timezone
ln -sf "/usr/share/zoneinfo/$TEST_TIMEZONE" /etc/localtime

exec "$@"
