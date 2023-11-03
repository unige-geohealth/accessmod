#!/bin/bash
#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#    
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# Script to build and push new image in remote docker repository
#
set -e

# Configuration
REMOTE="github"
IMAGENAME="fredmoser/accessmod"
CUR_DIR=$(pwd)
FILE_TESTS="/tmp/tests.log"
NEW_VERSION=$1
OLD_VERSION=$(<version.txt)
DRY_RUN=false
BRANCH=$(git rev-parse --abbrev-ref HEAD)
FILE_TESTS="/tmp/tests.json"

# Function to execute commands
run_cmd() {
  if [ "$DRY_RUN" = true ]; then
    echo "DRY RUN: $*"
  else
    eval "$*"
  fi
}

# Function to print with color
print_color() {
  local color=$1
  local text=$2
  local reset="\033[0m"
  echo -e "$color$text$reset"
}

# Logging function with timestamp
log() {
  echo "[$(date +'%Y-%m-%d %T')] $1"
}

# Exit with error message
error_exit() {
  print_color "\033[31m" "$1"
  exit 1
}

# Function to check for command dependencies
check_command() {
  if ! command -v "$1" &> /dev/null; then
    error_exit "Missing command $1"
  fi
}

# Check necessary commands
check_command 'git'
check_command 'docker'
check_command 'jq'

# Exclude vim from command checking, as it is not required for non-interactive (dry-run) mode
if [ "$DRY_RUN" = false ]; then
  check_command 'vim'
fi

# Process arguments
while (( "$#" )); do
  case "$1" in
    --dry-run)
      DRY_RUN=true
      ;;
    *)
      ;;
  esac
  shift
done

# Validate new version input
if [ -z "$NEW_VERSION" ] || [ "$NEW_VERSION" == "$OLD_VERSION" ]; then
  error_exit "Wrong or missing version. Old version: $OLD_VERSION, new version: $NEW_VERSION"
fi

# Check for uncommitted changes
if [ "$(git status --porcelain | wc -l)" -gt 0 ]; then
  error_exit "There are uncommitted changes. Stopping here."
fi

# Confirm build process
read -r -p "Build for multiarch and push (p), local (l), or cancel (c)? [p/l/c] " build_config
case "$build_config" in
  p|l)
    # Proceed with the script
    ;;
  *)
    log "Cancelled by user."
    exit 0
    ;;
esac

# Run tests
log "Running end to end tests..." 

# Define the output file for test results

# Run the R script with Docker, passing the output file as an argument
docker run -v "$(pwd)":/app "$IMAGE" Rscript tests/start.R "$FILE_TESTS"

if [ -s "$FILE_TESTS" ]; then
  TEST_RESULT=$(jq -r '.pass' < "$FILE_TESTS")
  if [ "$TEST_RESULT" != "true" ]; then
    error_exit "Tests failed, check logs at $FILE_TESTS"
  fi
  log "Tests passed successfully."
else
  error_exit "No test results found. The test may not have run correctly."
fi

exit 0;

# Update versions
log "Updating version.txt to $NEW_VERSION"
echo "$NEW_VERSION" > version.txt

# Only run vim if it's not a dry run
if [ "$DRY_RUN" = false ]; then
  vim changes.md
fi

# Build docker images
if [ "$build_config" == "p" ] && [ "$DRY_RUN" = false ]; then
  log "Building and pushing multiarch images"
  ./docker/build_docker.sh -pa
elif [ "$build_config" == "l" ] && [ "$DRY_RUN" = false ]; then
  log "Building local images"
  ./docker/build_docker.sh -la
fi

# Git operations
if [ "$DRY_RUN" = false ]; then
  log "Committing changes to Git"
  git add .
  git commit -m "version $NEW_VERSION"
  git tag "$NEW_VERSION"
  log "Final review of changes with a delay..."
  git --no-pager diff --minimal
  sleep 5  # Sleep reduced to 5s for quick review

  # Confirm push
  read -r -p "Are you sure you want to push to $REMOTE? (y/n): " confirm_push
  if [ "$confirm_push" != "y" ]; then
    log "Push cancelled by user."
    exit 0
  fi
  git push "$REMOTE" "$BRANCH" --tags
  log "Pushed to remote repository successfully."
fi

# In the end, notify the user if it was a dry run
if [ "$DRY_RUN" = true ]; then
  echo "Dry run completed. No changes were made."
else
  echo "Build script completed successfully."
fi

