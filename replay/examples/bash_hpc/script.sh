#!/bin/bash 

IMAGE='fredmoser/accessmod:5.7.21-alpha-1.2'
OUTPUT_DIR='./out'
PROJECT_FILE='./project.am5p'
SCRIPT_FILE='./script.R'
CONFIG_FILE='./config.json'
DATA_FILE="./data.json"

echo "Start processing AccessMod Job"

check_file()
{
  if [ ! -e "$1" ]; 
  then 
    echo "Missing file/dir: $1";
    exit 1;
  fi
}
check_file "$PROJECT_FILE"
check_file "$SCRIPT_FILE"
check_file "$CONFIG_FILE"
check_file "$DATA_FILE"
check_file "$OUTPUT_DIR"

docker run \
  -v $(pwd)/$OUTPUT_DIR:/batch/out \
  -v $(pwd)/$PROJECT_FILE:/batch/project.am5p \
  -v $(pwd)/$CONFIG_FILE:/batch/config.json \
  -v $(pwd)/$DATA_FILE:/batch/data.json \
  -v $(pwd)/$SCRIPT_FILE:/batch/script.R \
  $IMAGE \
  Rscript /batch/script.R 

