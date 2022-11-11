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
check_command 'packer'

echo "Build using packer"
VERSION=`cat version.txt`
DIR=./packer
PACKERCONF=$DIR/alpine.json
IMAGENAME=fredmoser/accessmod

jq '.variables.version = "'$NEW_VERSION'"' $PACKERCONF > /tmp/packer.json && mv /tmp/packer.json $PACKERCONF
jq '.variables.image = "'$IMAGENAME:$NEW_VERSION'"' $PACKERCONF > /tmp/packer.json && mv /tmp/packer.json $PACKERCONF

cd packer
./build.sh
packer build -force alpine.json
