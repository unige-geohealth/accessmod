#!/bin/bash

. $AM5_SCRIPTS_FOLDER/env.sh
. $AM5_SCRIPTS_FOLDER/message.sh 
. $AM5_SCRIPTS_FOLDER/helpers.sh 


HELP_NAV="keys UP/DOWN to choose, LEFT/RIGHT to confirm/cancel"


_fetch() {
  VERSIONS_RAW=$(wget -O - "$AM5_DOCKER_API_URL")
  echo $VERSIONS_RAW > $VERSIONS_CACHE_FILE
  echo $VERSIONS_RAW
}

# Returns list of versions such as :
# 0 latest 1 5.7.16 2 5.7.15 
# which is a correct format used in dialog --menu 
_list_num(){
  OUT=$( _versions_data | jq -r '
  .results
  | map( .name )
  | to_entries
  | map(((.key | tostring) + " " + .value))
  | join(" ")')


  if [ -z "$OUT" ]
  then
    OUT="0 latest"
  fi

  echo $OUT
}

# Return index of version ( match index from  _list_num)
# param $1 index number
_list_index(){
  pos=$1
  if [ -z $pos ]
  then
    pos=0
  fi

  OUT=$( _versions_data | jq --arg pos $pos -r '
  .results
  | map( .name )
  | .[ $pos | tonumber ]')

  if [[ -z "$OUT" || "$OUT" = "null" || "$?" != "0" ]]
  then
    OUT="latest"
  fi
  echo  $OUT
}

# Current version (file) 
_version_current(){
  ver=$(cat $AM5_VERSION_FILE)
  echo $ver
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


# Display dialog menu, with list of versions to choose
_versions () {
  dialog \
    --backtitle "$BACKTITLE" \
    --menu "Select version: \n $HELP_NAV " $HEIGHT $WIDTH 10 \
    $(_list_num ) 2> $TMP_FILE

  if [ "$?" != "0" ]
  then
    _main
  else
    _update
  fi

}

# Update / change version
_update(){
  pos=$( cat $TMP_FILE )
  ver=$( _list_index $pos )

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
    echo "$ver" > $AM5_VERSION_FILE
    AM5_VERSION=$ver
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
  ver=$(cat $AM5_VERSION_FILE)
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

_stop_server(){
  RUNNING=`docker ps -qa --filter name=$AM5_NAME`

  if [[ -n "$RUNNING" ]]
  then
    _msg "Stop container $AM5_NAME"
    docker stop $RUNNING
    docker rm $RUNNING
  fi

}

# Welcom panel
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
        "0" "Change / update version" \
        "1" "Fetch remote versions" \
        "2" "Stop the server" \
        "3" "Start/Restart the server" \
        "4" "Stop the virtual machine"\
        "5" "Remove old versions" 2> $TMP_FILE

      if [ "$?" != "0" ]
      then
        dialog --clear
        echo "Have a nice day ! ( type 'menu' to reopen the menu )"
      else
        res=$(cat $TMP_FILE)
        case "$res" in
          0)
            _versions
            ;;
          1)
            _fetch
            _versions
            ;;
          2)
            _stop_server
            _main
            ;;
          3)
            _start
            _main
            ;;
          4)
            _poweroff
            ;;
          5) 
            _remove_old_images
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


