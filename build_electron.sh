#!/bin/bash 
check_command()
{
  if [ -z `command -v $1` ]; 
  then 
    echo "Missing command $1";
    exit 1;
  fi
}
check_command 'jq'
check_command 'yarn'
check_command 'docker'

VERSION=`cat version.txt`
DIR=./electron
PACKAGE=$DIR/package.json

jq '.version = "'$VERSION'"' $PACKAGE > /tmp/package.json && mv /tmp/package.json $PACKAGE

cd $DIR
yarn run make
