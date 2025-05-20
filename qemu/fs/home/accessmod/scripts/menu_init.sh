#!/bin/bash

. $AM5_SCRIPTS_FOLDER/env.sh
. $AM5_SCRIPTS_FOLDER/message.sh 
. $AM5_SCRIPTS_FOLDER/helpers.sh 


HELP_NAV="keys UP/DOWN to choose, LEFT/RIGHT to confirm/cancel"


_fetch() {
  # Use "5" instead of $AM5_MIN_VERSION to get all versions starting with 5
  API_URL="${AM5_DOCKER_HUB}/tags/?page_size=100&page=1&name=5"
  VERSIONS_RAW=$(wget -O - "$API_URL")
  echo $VERSIONS_RAW > $VERSIONS_CACHE_FILE
  _msg "Remote versions fetched and cached" --duration 2
}

# Returns list of production versions (no alpha/beta) formatted for radiolist
_list_production_versions(){
  OUT=$( _versions_data | jq -r '
  .results
  | map( .name )
  | map(select(. == "latest" or ((startswith("5.8") or startswith("5.9")) and (contains("-alpha") | not) and (contains("-beta") | not))))
  | map(. + " " + . + " off")  # Format: "tag item status"
  | join(" ")')
  
  if [ -z "$OUT" ]
  then
    OUT="latest latest on"
  fi
  
  echo $OUT
}

# Returns list of all versions (including alpha/beta) formatted for radiolist
_list_all_versions(){
  OUT=$( _versions_data | jq -r '
  .results
  | map( .name )
  | map(select(. == "latest" or startswith("5.8") or startswith("5.9")))
  | map(. + " " + . + " off")  # Format: "tag item status"
  | join(" ")')
  
  if [ -z "$OUT" ]
  then
    OUT="latest latest on"
  fi
  
  echo $OUT
}


# Current version
_version_current(){
  _get_version
}

# Return version object
_versions_data(){
  if [ -e "$VERSIONS_CACHE_FILE" ]
  then
    VERSIONS_RAW=$(cat $VERSIONS_CACHE_FILE)
  else
    VERSIONS_RAW=$(_fetch)
  fi
  echo $VERSIONS_RAW
}


# Display dialog radiolist with production versions
_versions_production() {
  dialog \
    --backtitle "$BACKTITLE" \
    --radiolist "Select production version: \n $HELP_NAV " $HEIGHT $WIDTH 10 \
    $(_list_production_versions) 2> $TMP_FILE

  if [ "$?" != "0" ]
  then
    _main
  else
    _update
  fi
}

# Display dialog radiolist with all versions (including alpha/beta)
_versions_all() {
  dialog \
    --backtitle "$BACKTITLE" \
    --radiolist "Select any version (including alpha/beta): \n $HELP_NAV " $HEIGHT $WIDTH 10 \
    $(_list_all_versions) 2> $TMP_FILE

  if [ "$?" != "0" ]
  then
    _main
  else
    _update
  fi
}

# Update / change version
_update(){
  ver=$( cat $TMP_FILE )

  dialog \
    --backtitle "$BACKTITLE" \
    --clear \
    --ok-label "Confirm" \
    --no-label "Ignore" \
    --yesno "Confirm set AccessMod version to $ver " $HEIGHT $WIDTH

  if [ "$?" != "0" ]
  then
    _main
  else
    _set_version "$ver"
    _start
    _main
  fi
}

# Power off the virtual machine
_poweroff(){
  dialog \
    --backtitle "$BACKTITLE" \
    --clear \
    --ok-label "Confirm" \
    --no-label "Cancel" \
    --yesno "Confirm power off the machine" $HEIGHT $WIDTH

  if [ "$?" != "0" ]
  then
    _main
  else
    sudo poweroff
  fi
}

# (re) Start docker with curren version ( based on the version stored in file ) 
_start(){
  sh $AM5_SCRIPTS_FOLDER/start.sh
}

# Clean version older than current
_remove_old_images(){
  ver=$(_get_version)
  img="$AM5_REPO:$ver"
  old_images=$(docker images -aq --filter before=$img)

  if [[ -z "$old_images" ]]
  then
    _msg "No image to remove" --duration 2
    _main 
  else
    old_images_txt=$(docker images -a --filter before=$img)
    dialog \
      --backtitle "$BACKTITLE" \
      --clear \
      --ok-label "Confirm" \
      --no-label "Ignore" \
      --yesno "Remove old images:\n $old_images_txt" $HEIGHT $WIDTH

    if [ "$?" != "0" ]
    then
      _main
    else
      docker rmi $old_images
      _main
    fi
  fi
}


_welcome_message(){
  if _check_server_health; then
    # Server is healthy
    echo  "Welcome to AccessMod $ver\n\n\
      The application should be available at\n\
      http://localhost:$AM5_PORT_APP_PUBLIC\n\
      "
  else
     # Server is not healthy
    echo  "Welcome to AccessMod $ver\n\n\
      The server is not running. Start/Restart it?\n\
      "
  fi
}

# Function to refresh the menu and re-check server status
_refresh_status() {
  _msg "Refreshing server status..." --duration 1
  _main
}

_stop_server(){
  RUNNING=`docker ps -qa --filter name=$AM5_NAME`

  if [[ -n "$RUNNING" ]]
  then
    _msg "Stop container $AM5_NAME"
    docker stop $RUNNING
    docker rm $RUNNING
  fi

}

# Welcome panel
_welcome(){
  ver=$(_version_current)
  msg=$(_welcome_message)
  dialog \
    --backtitle "$BACKTITLE" \
    --clear \
    --ok-label "Select" \
    --cancel-label "Quit -> login" \
    --menu "\
    $msg \n\
    $HELP_NAV
      " $HEIGHT $WIDTH 10 \
        "0" "Change version: production" \
        "1" "Change version: all" \
        "2" "Fetch remote versions" \
        "3" "Stop the server" \
        "4" "Start/Restart the server" \
        "5" "Stop the virtual machine"\
        "6" "Remove old versions" \
        "7" "Refresh status" 2> $TMP_FILE

      if [ "$?" != "0" ]
      then
        dialog --clear
        echo "Have a nice day ! ( type 'menu' to reopen the menu )"
      else
        res=$(cat $TMP_FILE)
        case "$res" in
          0)
            _versions_production
            ;;
          1)
            _versions_all
            ;;
          2)
            _fetch
            _main
            ;;
          3)
            _stop_server
            _main
            ;;
          4)
            _start
            _main
            ;;
          5)
            _poweroff
            ;;
          6) 
            _remove_old_images
            ;;
          7)
            _refresh_status
            ;;
          *)
            _main
            ;;
        esac
      fi
    }

  # Main program
_main(){
  _welcome
}

if _check_server_health; then
   echo "Server ok "
else
   _start
fi


_main
