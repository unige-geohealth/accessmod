#!/bin/bash 

IMAGE='fredmoser/accessmod:latest'
#docker compose exec am5_dev Rscript tests/start.R

docker run $IMAGE Rscript tests/start.R
