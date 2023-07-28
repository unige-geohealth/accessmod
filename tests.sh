#!/bin/bash 


#
# Dev mode when docker compose is up
#
# docker compose exec am5_dev Rscript tests/start.R

#
# New docker container 
#
IMAGE='fredmoser/accessmod:latest'
docker run -v $(pwd):/app $IMAGE Rscript tests/start.R
