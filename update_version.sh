#!/bin/bash

#
# Script to build and push new image in remote docker repository
#
BRANCH=$(git branch | grep '*' |awk '{ print $2}')
REMOTE=origin
NEW_VERSION=$1
OLD_VERSION=`cat version.txt`
echo $NEW_VERSION
FG_GREEN="\033[32m"
FG_NORMAL="\033[0m"
FG_RED="\033[31m"
CHANGES_CHECK=$(git status --porcelain | wc -l)
CUR_HASH=$(git rev-parse HEAD)
USAGE="Usage : bash update_version.sh $OLD_VERSION"

if [ $CHANGES_CHECK -gt 0 ]
then 
  echo -e "This project as $FG_RED$CHANGES_CHECK uncommited changes$FG_NORMAL stop here"
  exit 1
fi

if [ -z "$NEW_VERSION" ] || [ "$NEW_VERSION" == "$OLD_VERSION" ]
then
  echo -e "Wrong or missing version. Old version version =  $FG_RED$OLD_VERSION$FG_NORMAL new version = $FG_GREEN$NEW_VERSION$FG_NORMAL"
  echo "$USAGE"
  exit 1
fi

#
# Confirm start
#
echo -e "This script will produce a new version $FG_GREEN$NEW_VERSION$FG_NORMAL and push it on branch $FG_GREEN$BRANCH$FG_NORMAL. Continue ? [YES/NO]"
read confirm_start

if [ "$confirm_start" != "YES"  ]
then 
  echo "Cancel" 
  exit 1
fi

#
# Update version + verification
#
echo "Update version.txt"
REP_VERSION='s/'"$OLD_VERSION"'/'"$NEW_VERSION"'/g'
perl -pi -e "$REP_VERSION"  version.txt

echo "Write changes"
vim changes.md

echo "Get diff"
git --no-pager diff --minimal

echo "Verify git diff of versioning changes. Continue ? [YES/NO]"
read confirm_diff

if [ "$confirm_diff" != "YES"  ]
then 
  echo "Stop here, stash changes. rollback to $CUR_HASH " 
  git stash
  exit 1
fi

git add .
git commit -m "version $NEW_VERSION"
git tag $NEW_VERSION
git push $REMOTE $BRANCH --tags

echo "Done"

