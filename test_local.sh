#!/bin/bash

#
# Local-only regression tests.
# These tests may rely on large fixtures mounted through docker compose,
# especially /data/shared, and are not intended for CI.

set -e

docker compose exec -T am5_dev Rscript tests/local/start.R /tmp/tests_local.json
