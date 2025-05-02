#!/bin/bash

_check_server_health() {
  response=$(wget --timeout=5 -qO - "$HEALTH_URL" 2>/dev/null) 
  if [ "$response" == "ok" ]; then
    return 0 # Server is working/healthy
  else
    return 1  # Server is failed/not healthy
  fi
}

_check_server_health_text(){
  if _check_server_health ; then
    echo "ok"
  else
    echo "fail"  
  fi
}
