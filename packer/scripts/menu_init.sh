
AM5_DOCKER_HUB="${AM5_HUB_API}/repositories/$AM5_REPO"
AM5_DOCKER_API_URL="$AM5_DOCKER_HUB/tags/?page_size=10&page=1&name=$MINVERSION"
AM5_VERSION=$(cat $AM5_VERSION_FILE)

AM5_NAME="accessmod"
AM5_VERSION_ORIG=$AM5_VERSION
AM5_VERSION=$(cat $AM5_VERSION_FILE)
AM5_IMAGE="$AM5_REPO:$AM5_VERSION"

if [ -z "$AM5_VERSION" ]
then
  $AM5_VERSION=$AM5_VERSION_ORIG
fi


WIDTH=80
HEIGHT=30
BACKTITLE="AccessMod"
VERSIONS_RAW="";
VERSIONS_REMOTE=""
VERSIONS_CACHE_FILE="/tmp/versions.json"
TMP_FILE="/tmp/res"
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

# Display a message ($1) and wait ($2)
_msg(){
  s=$2
  if [ -z "$s" ]
  then 
    s=2
  fi

  if [ -n ]
  then
    dialog \
      --backtitle "$BACKTITLE" \
      --title "$TITLE" \
      --infobox "$1" 3 $WIDTH
          sleep $s 
        else
          echo $1
  fi
}

# Display dialog menu, with list of versions to choose
_versions () {
  dialog \
    --backtitle $BACKTITLE \
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
    --backtitle $BACKTITLE \
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
    --backtitle $BACKTITLE \
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
  dialog --clear
  sudo sh $AM5_SCRIPTS_FOLDER/start.sh
}

# Clean version older than current
_remove_old_images(){
  ver=$(cat $AM5_VERSION_FILE)
  img="$AM5_REPO:$ver"
  old_images=$(sudo docker images -aq --filter before=$img)

  if [[ -z "$old_images" ]]
  then
    _msg "No image to remove" 4
    _main 
  else
    old_images_txt=$(sudo docker images -a --filter before=$img)
    dialog \
      --backtitle $BACKTITLE \
      --clear \
      --ok-label "Confirm" \
      --no-label "Ignore" \
      --yesno "Remove old images:\n $old_images_txt" $HEIGHT $WIDTH

    if [ "$?" != "0" ]
    then
      _main
    else
      sudo docker rmi $old_images
      _main
    fi
  fi
}

# Welcom panel
_welcome(){
  ver=$(_version_current)
  dialog \
    --backtitle $BACKTITLE \
    --clear \
    --ok-label "Select" \
    --cancel-label "Quit -> login" \
    --menu "\
    Welcome to AccessMod $ver \n\
    The application is available at \n\
    http://localhost:8080 \n\
    $HELP_NAV
      " $HEIGHT $WIDTH 10 \
        "0" "Change / update version" \
        "1" "Fetch remote versions" \
        "2" "Restart the server" \
        "3" "Stop the machine"\
        "4" "Remove old versions" 2> $TMP_FILE

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
            _start
            _main
            ;;
          3)
            _poweroff
            ;;
          4) 
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

_main


