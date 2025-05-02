#!/bin/sh
gh workflow run build_qemu.yml -f version=${1:-5.8}
