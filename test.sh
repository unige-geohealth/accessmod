#!/bin/bash

#
# NOTE: Used in 
# - github action (could break relase)
# - local npm run test

set -e
# shared file
FILE_TESTS="/tmp/tests.json"
# Set defaults and allow overrides from environment variables
DOCKER_REPO=${DOCKER_REPO:-fredmoser}
DOCKER_NAME=${DOCKER_NAME:-accessmod}
DOCKER_TAG=${DOCKER_TAG:-latest}
IMAGENAME="$DOCKER_REPO/$DOCKER_NAME:$DOCKER_TAG"

# Execute tests and write result in shared file  
docker run -v /tmp:/tmp\
  -v "$(pwd)":/app \
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
