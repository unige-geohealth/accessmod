#!/bin/bash

#
# NOTE: Used in
# - github action (could break release)
# - local npm run test

set -e
# shared file
FILE_TESTS="/tmp/tests.json"
# Set defaults and allow overrides from environment variables
DOCKER_REPO=${DOCKER_REPO:-fredmoser}
DOCKER_NAME=${DOCKER_NAME:-accessmod_base}
DOCKER_TAG=${DOCKER_TAG:-5.9-d}
IMAGENAME="$DOCKER_REPO/$DOCKER_NAME:$DOCKER_TAG"
TEST_VOL="accessmod-test-db"

# Ensure volume is cleaned up on exit (success or failure)
trap 'docker volume rm "$TEST_VOL" 2>/dev/null || true' EXIT

# 1. Create a fresh writable copy of the GRASS DB in a Docker volume.
#    Source: local ./docker/alpine_base/data/ (same content as what ADD bakes
#    into the image — no post-build RUN steps modify it).
#    Mounting the volume at /data/dbgrass shadows the image layer, allowing
#    GRASS to rename vector directories (overlayfs prevents this on lower layers).
docker volume rm "$TEST_VOL" 2>/dev/null || true
docker volume create "$TEST_VOL"
docker run --rm \
  -v "$(pwd)/docker/alpine_base/data:/source:ro" \
  -v "$TEST_VOL:/data/dbgrass" \
  "$IMAGENAME" \
  sh -c 'cp -a /source/. /data/dbgrass/'

# 2. Execute tests with the writable volume mounted at the same path as GISDBASE.
#    No GISDBASE override needed: volume shadows the image layer transparently.
docker run -v /tmp:/tmp \
  -v "$(pwd)":/app \
  -v "$TEST_VOL:/data/dbgrass" \
  "$IMAGENAME" \
  Rscript tests/start.R "$FILE_TESTS"

if [ -s "$FILE_TESTS" ]; then
  TEST_RESULT=$(jq -r '.pass' < "$FILE_TESTS")
  if [ "$TEST_RESULT" != "true" ]; then
    echo "Tests failed, check logs"
    cat $TEST_RESULT
    exit 1
  fi
  echo "Tests passed successfully."
else
  echo "No test results found. The test may not have run correctly."
  exit 1
fi
