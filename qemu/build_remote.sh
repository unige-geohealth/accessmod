#!/bin/bash

cur=$(git branch --show-current)
gh workflow run build_qemu --ref $cur -f version=${1:-5.9}
