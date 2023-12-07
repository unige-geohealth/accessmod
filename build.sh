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

# Semantic Version comparison function
compare_versions() {
  if [[ $1 == $2 ]]
  then
    echo "equal"
  elif [[ $1 == $(echo -e "$1\n$2" | sort -V | head -n1) ]]
  then
    echo "lesser"
  else
    echo "greater"
  fi
}

# Function to check if the version is well-formed according to SemVer
is_well_formed_semver() {
  local semver_regex="^[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?(\+[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?$"
  if [[ $1 =~ $semver_regex ]]
  then
    return 0
  else
    return 1
  fi
}

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


# Check necessary commands
check_command 'git'
check_command 'docker'
check_command 'jq'

# Exclude vim from command checking, as it is not required for non-interactive (dry-run) mode
if [ "$DRY_RUN" = false ]; then
  check_command 'vim'
fi

# Check if the new version is well-formed
if ! is_well_formed_semver "$NEW_VERSION"; then
  error_exit "The new version ($NEW_VERSION) is not well-formed SemVer."
fi

# Validate new version input
if [[ $(compare_versions "$NEW_VERSION" "$OLD_VERSION") != "greater" ]]; then
  error_exit "New version ($NEW_VERSION) must be greater than the old version ($OLD_VERSION)."
fi

# Check for uncommitted changes unless in DRY_RUN mode
if [ "$DRY_RUN" = false ]; then
  if [ "$(git status --porcelain | wc -l)" -gt 0 ]; then
    error_exit "There are uncommitted changes. Stopping here."
  fi
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


# Run the R script with Docker, passing the output file as an argument
docker run -v /tmp:/tmp\
  -v "$(pwd)":/app \
  "$IMAGENAME":latest\
  Rscript tests/start.R \
  "$FILE_TESTS"

if [ -s "$FILE_TESTS" ]; then
  TEST_RESULT=$(jq -r '.pass' < "$FILE_TESTS")
  if [ "$TEST_RESULT" != "true" ]; then
    error_exit "Tests failed, check logs at $FILE_TESTS"
  fi
  log "Tests passed successfully."
else
  error_exit "No test results found. The test may not have run correctly."
fi

# Update versions
log "Updating version.txt to $NEW_VERSION"
if [ "$DRY_RUN" = false ]; then
  echo "$NEW_VERSION" > version.txt
fi

log "Edit changes..."
# Only run vim if it's not a dry run
if [ "$DRY_RUN" = false ]; then
  vim changes.md
fi

# Build docker images

if [ "$build_config" == "p" ]; then
  log "Building and pushing multiarch images"
  if [ "$DRY_RUN" = false ]; then
    ./docker/build_docker.sh -pa
  fi
elif [ "$build_config" == "l" ]; then
  log "Building local images"
  if [ "$DRY_RUN" = false ]; then
    ./docker/build_docker.sh -la
  fi
fi

# Git operations
log "Committing changes to Git"
if [ "$DRY_RUN" = false ]; then
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
  log "Dry run completed. No changes were made."
else
  log "Build script completed successfully."
fi

