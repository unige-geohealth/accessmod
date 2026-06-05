#!/bin/bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_DIR="$ROOT_DIR/qemu/tests"

echo "Syntax checking VM scripts..."
bash -n "$ROOT_DIR"/qemu/fs/home/accessmod/scripts/*.sh "$TEST_DIR"/*.sh

mapfile -t tests < <(find "$TEST_DIR" -maxdepth 1 -name "*.test.sh" -type f | sort)

if [[ ${#tests[@]} -eq 0 ]]; then
  echo "No qemu tests found."
  exit 1
fi

for test_file in "${tests[@]}"; do
  echo "Running ${test_file#$ROOT_DIR/}"
  bash "$test_file"
done

echo "QEMU script tests passed."
