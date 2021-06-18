#!/bin/sh 
set -e

newProj=$1
archiveFile=$2
dbPath=$3
tmpFolder=$(mktemp -d)

#
# Busybox zip 
#
# oldProj=$(unzip -l -q -p $archiveFile | awk 'NR==1{print $4}')

#
# UnZip 6.00 of 20 April 2009
#
oldProj=$(unzip -lq $archiveFile |  awk 'NR==3{print $4}')

if [ -n $oldProj ]; then
  unzip $archiveFile -d $tmpFolder
  mv $tmpFolder/$oldProj/$oldProj $tmpFolder/$oldProj/$newProj
  mv $tmpFolder/$oldProj $tmpFolder/$newProj
  mv $tmpFolder/$newProj $dbPath/$newProj 
fi
