#!/bin/bash

# create docker volumes
_ensure_docker_volumes() {
  docker volume create am_data_cache
  docker volume create am_data_logs
  docker volume create am_data_grass
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  _ensure_docker_volumes
fi
