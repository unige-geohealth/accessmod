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

#
# Check dependencies
#
check_command()
{
  if [ -z `command -v $1` ]; 
  then 
    echo "Missing command $1";
    exit 1;
  fi
}
check_command 'jq'
check_command 'packer'
check_command 'git'
check_command 'docker'
check_command 'yarn'
check_command 'vim'

#
# Set variables
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
USAGE="Usage : bash build.sh $OLD_VERSION"
PACKAGE=./electron/package.json
PACKERCONF=./packer/alpine.json
IMAGENAME="fredmoser/accessmod"
CUR_DIR=$(pwd)
FILE_TESTS="/tmp/tests.log"
TEST_SUCCESS_STRING="Success!"


if [ -z "$NEW_VERSION" ] || [ "$NEW_VERSION" == "$OLD_VERSION" ]
then
  echo -e "Wrong or missing version. Old version version =  $FG_RED$OLD_VERSION$FG_NORMAL new version = $FG_GREEN$NEW_VERSION$FG_NORMAL"
  echo "$USAGE"
  exit 1
fi

if [ $CHANGES_CHECK -gt 0 ]
then 
  echo -e "This project as $FG_RED$CHANGES_CHECK uncommited changes$FG_NORMAL stop here"
  exit 1
fi


#
# Confirm start
#
echo -e "This script will produce a new version $FG_GREEN$NEW_VERSION$FG_NORMAL and optionally push it on branch $FG_GREEN$BRANCH$FG_NORMAL. Continue ? [y/n]"
read confirm_start

if [ "$confirm_start" != "y"  ]
then 
  echo "Cancel" 
  exit 1
fi

#
# Update version + verification
#
echo "Update version.txt and packages"
echo $NEW_VERSION > version.txt

echo "Build local and test  [y/n]"
read confirm_test

if [ "$confirm_test" == "y"  ]
then 
  echo "Build local"
  ./docker/build_docker.sh -la
  echo "End to end testing" 
  #
  # Testing GRASS + R 
  # TODO: Parse test results instead of grep for "success string" 
  #
  docker run $IMAGENAME:$NEW_VERSION Rscript tests/start.R &> $FILE_TESTS 
  TT=$(cat $FILE_TESTS | grep "$TEST_SUCCESS_STRING")
  if [[ -z $TT ]]
  then
    echo "Tests failed, check logs at $FILE_TESTS"
    exit 1
  else 
    echo $TEST_SUCCESS_STRING
  fi
fi

echo "Write changes"
vim changes.md

echo "Get diff"
git --no-pager diff --minimal

echo "Verify git diff of versioning changes. Continue ? [y/n]"
read confirm_diff

if [ "$confirm_diff" != "y"  ]
then 
  echo "Stop here, stash changes. rollback to $CUR_HASH " 
  git stash
  exit 1
fi

echo "Build docker images : dry multiarch + push"
./docker/build_docker.sh -p

echo "Build multiarch and push (p), local (l), skip (s)  [p/l/s]"
read confirm_push_docker

if [ "$confirm_push_docker" == "p"  ]
then
  echo "Build and push multiarch and push"
  ./docker/build_docker.sh -pa
fi

if [ "$confirm_push_docker" == "l"  ]
then
  echo "Build local"
  ./docker/build_docker.sh -la
fi

echo "Build electron app:  build (b), skip (s)  [b/s]"
read confirm_electron

if [ "$confirm_electron" == "b" ]
then
  echo "Build electron app"
  jq '.version = "'$NEW_VERSION'"' $PACKAGE > /tmp/package.json && mv /tmp/package.json $PACKAGE
  cd ./electron
  yarn run make
  cd $CUR_DIR
fi

echo "Build packer app (VM) (require x86 + virtualbox):  build (b), skip (s)  [b/s]"
read confirm_packer

if [ "$confirm_packer" == "b" ]
then
  echo "Build using packer"
  jq '.variables.version = "'$NEW_VERSION'"' $PACKERCONF > /tmp/packer.json && mv /tmp/packer.json $PACKERCONF
  jq '.variables.image = "'$IMAGENAME:$NEW_VERSION'"' $PACKERCONF > /tmp/packer.json && mv /tmp/packer.json $PACKERCONF
  cd packer
  ./build.sh
  packer build -force alpine.json
  cd $CUR_DIR
fi

echo "Git commit"
git add .
git commit -m "version $NEW_VERSION"
git tag $NEW_VERSION


echo "Push code to remote ? [y/n]"
read confirm_push_git
if [ "$confirm_push_git" == "y" ]
then
  echo "Push to git"
  git push $REMOTE $BRANCH --tags
else
  echo "Command for manual push :"
  echo "git push $REMOTE $BRANCH --tags "
fi

echo "Done"
