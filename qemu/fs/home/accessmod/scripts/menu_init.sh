#!/bin/bash

# Source environment variables directly
. /etc/profile.d/am5_env.sh

# Source required scripts
source "$AM5_SCRIPTS_FOLDER/env.sh"
source "$AM5_SCRIPTS_FOLDER/message.sh"
source "$AM5_SCRIPTS_FOLDER/helpers.sh"

HELP_NAV="Use UP/DOWN to choose, SPACE to select, LEFT/RIGHT to confirm/cancel"

_fetch() {
  local api_url="${AM5_DOCKER_HUB}/tags/?page_size=100&page=1&name=5"
  local versions_raw

  versions_raw=$(wget -O - "$api_url")
  echo "$versions_raw" > "$VERSIONS_CACHE_FILE"
  _msg "Remote versions fetched and cached" --duration 2
  echo "$versions_raw"  # Return the fetched data
}

_list_production_versions() {
  local out
  out=$(_versions_data | jq -r '
    .results
    | map(.name)
    | map(select(. == "latest" or ((startswith("5.8") or startswith("5.9")) and (contains("-alpha") | not) and (contains("-beta") | not))))
    | map(. + " " + . + " off")
    | join(" ")'
  )

  [[ -z "$out" ]] && out="latest latest on"
  echo "$out"
}

_list_all_versions() {
  local out
  out=$(_versions_data | jq -r '
    .results
    | map(.name)
    | map(select(. == "latest" or startswith("5.8") or startswith("5.9")))
    | map(. + " " + . + " off")
    | join(" ")'
  )

  [[ -z "$out" ]] && out="latest latest on"
  echo "$out"
}

_version_current() {
  _get_version
}

_versions_data() {
  local versions_raw

  if [[ -e "$VERSIONS_CACHE_FILE" ]]; then
    versions_raw=$(<"$VERSIONS_CACHE_FILE")
  else
    versions_raw=$(_fetch)
  fi

  echo "$versions_raw"
}

_versions_production() {
  dialog \
    --backtitle "$BACKTITLE" \
    --radiolist "Select production version:\n$HELP_NAV" "$HEIGHT" "$WIDTH" 10 \
    $(_list_production_versions) 2> "$TMP_FILE"

  if [[ "$?" -ne 0 ]]; then
    _main
  else
    _update
  fi
}

_versions_all() {
  dialog \
    --backtitle "$BACKTITLE" \
    --radiolist "Select any version (including alpha/beta):\n$HELP_NAV" "$HEIGHT" "$WIDTH" 10 \
    $(_list_all_versions) 2> "$TMP_FILE"

  if [[ "$?" -ne 0 ]]; then
    _main
  else
    _update
  fi
}

_update() {
  local ver
  ver=$(<"$TMP_FILE")

  dialog \
    --backtitle "$BACKTITLE" \
    --clear \
    --ok-label "Confirm" \
    --no-label "Ignore" \
    --yesno "Confirm setting AccessMod version to $ver" "$HEIGHT" "$WIDTH"

  if [[ "$?" -ne 0 ]]; then
    _main
  else
    _set_version "$ver"
    _start
    _main
  fi
}

_poweroff() {
  dialog \
    --backtitle "$BACKTITLE" \
    --clear \
    --ok-label "Confirm" \
    --no-label "Cancel" \
    --yesno "Confirm power off the machine?" "$HEIGHT" "$WIDTH"

  if [[ "$?" -ne 0 ]]; then
    _main
  else
    sudo poweroff
  fi
}

_start() {
  bash "$AM5_SCRIPTS_FOLDER/start.sh"
}

_remove_old_images() {
  local ver img old_images old_images_txt
  ver=$(_get_version)
  img="$AM5_REPO:$ver"
  old_images=$(docker images -aq --filter before="$img")

  if [[ -z "$old_images" ]]; then
    _msg "No image to remove" --duration 2
    _main
  else
    old_images_txt=$(docker images -a --filter before="$img")
    dialog \
      --backtitle "$BACKTITLE" \
      --clear \
      --ok-label "Confirm" \
      --no-label "Ignore" \
      --yesno "Remove old images:\n$old_images_txt" "$HEIGHT" "$WIDTH"

    if [[ "$?" -ne 0 ]]; then
      _main
    else
      docker rmi $old_images
      _main
    fi
  fi
}

_welcome_message() {
  if _check_server_health; then
    echo -e "Welcome to AccessMod $( _version_current )\n\nThe application should be available at\nhttp://localhost:$AM5_PORT_APP_PUBLIC"
  else
    echo -e "Welcome to AccessMod $( _version_current )\n\nThe server is not running. Start/Restart it?"
  fi
}

_refresh_status() {
  _msg "Refreshing server status..." --duration 1
  _main
}

_stop_server() {
  local running
  running=$(docker ps -qa --filter name="$AM5_NAME")

  if [[ -n "$running" ]]; then
    _msg "Stopping container $AM5_NAME"
    docker stop "$running"
    docker rm "$running"
  fi
}

_welcome() {
  local ver msg res
  ver=$(_version_current)
  msg=$(_welcome_message)

  dialog \
    --backtitle "$BACKTITLE" \
    --clear \
    --ok-label "Select" \
    --cancel-label "Quit -> login" \
    --menu "$msg\n\n$HELP_NAV" "$HEIGHT" "$WIDTH" 10 \
    "0" "Change version: production" \
    "1" "Change version: all" \
    "2" "Fetch remote versions" \
    "3" "Stop the server" \
    "4" "Start/Restart the server" \
    "5" "Stop the virtual machine" \
    "6" "Remove old versions" \
    "7" "Refresh status" 2> "$TMP_FILE"

  if [[ "$?" -ne 0 ]]; then
    dialog --clear
    echo "Have a nice day! (type 'menu' to reopen the menu)"
  else
    res=$(<"$TMP_FILE")
    case "$res" in
      0) _versions_production ;;
      1) _versions_all ;;
      2) _fetch; _main ;;
      3) _stop_server; _main ;;
      4) _start; _main ;;
      5) _poweroff ;;
      6) _remove_old_images ;;
      7) _refresh_status ;;
      *) _main ;;
    esac
  fi
}

_main() {
  _welcome
}

if _check_server_health; then
  echo "Server OK"
else
  _start
fi

_main
