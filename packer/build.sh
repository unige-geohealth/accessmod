#! /bin/bash
set -e

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
check_command 'docker'

VM_VERSION=5.8 
AM5_IMAGE="fredmoser/accessmod"
AM5_VERSION=`docker run --rm $AM5_IMAGE:latest cat version.txt`
ARCHIVE_DIR=./image_archive
ARCHIVE_PATH=$ARCHIVE_DIR/accessmod-docker.tar.gz
PACKER_CONF=./alpine.json
ISO_URL="https://dl-cdn.alpinelinux.org/alpine/v3.14/releases/x86_64/alpine-virt-3.14.8-x86_64.iso"
ISO_LOCAL="iso/alpine-virt-3.14.8-x86_64.iso"
ISO_CHECKSUM="2f46f2340ba82763e10984a56dd4bd9002f8cd12f45d617bb81a7597d2033250"

if [[ -z $AM5_VERSION ]]
then 
  echo "no version"
  exit 1
fi

if [[ ! -e $PACKER_CONF ]]
then 
  echo "no packer conf $PACKER_CONF"
  exit 1
fi

if [[ -e $ARCHIVE_PATH ]]
then 
  echo "rm previous archive"
  rm $ARCHIVE_PATH 
fi

mkdir -p $ARCHIVE_DIR

docker pull $AM5_IMAGE:$AM5_VERSION 
docker save $AM5_IMAGE:$AM5_VERSION > $ARCHIVE_PATH

jq '.variables.vm_version = "'$VM_VERSION'"' $PACKER_CONF >\
  /tmp/packer.json &&\
  mv /tmp/packer.json $PACKER_CONF

jq '.variables.version = "'$AM5_VERSION'"' $PACKER_CONF >\
  /tmp/packer.json &&\
  mv /tmp/packer.json $PACKER_CONF

jq '.variables.image = "'$AM5_IMAGE:$AM5_VERSION'"' $PACKER_CONF >\
  /tmp/packer.json &&\
  mv /tmp/packer.json $PACKER_CONF

jq '.variables.iso_download_url = "'$ISO_URL'"' $PACKER_CONF >\
  /tmp/packer.json &&\
  mv /tmp/packer.json $PACKER_CONF

jq '.variables.iso_local_url = "'$ISO_LOCAL'"' $PACKER_CONF >\
  /tmp/packer.json &&\
  mv /tmp/packer.json $PACKER_CONF

jq '.variables.iso_checksum = "'$ISO_CHECKSUM'"' $PACKER_CONF >\
  /tmp/packer.json &&\
  mv /tmp/packer.json $PACKER_CONF



packer build -force $PACKER_CONF



