#!/bin/bash

# Source required scripts
. "$AM5_SCRIPTS_FOLDER/env.sh"
. "$AM5_SCRIPTS_FOLDER/message.sh"
. "$AM5_SCRIPTS_FOLDER/helpers.sh"
. "$AM5_SCRIPTS_FOLDER/update.sh"

HELP_NAV="Use UP/DOWN to navigate, ENTER to select"

_fetch() {
  local api_url="${AM5_DOCKER_API_URL}"
  local versions_raw
  local fetch_error
  local curl_error=""
  local wget_error=""

  _msg "Fetching remote versions. Please wait..." --duration 1 >&2

  if command -v curl >/dev/null 2>&1; then
    fetch_error=$(mktemp)
    if versions_raw=$(curl -fsSL --connect-timeout 10 --max-time 30 "$api_url" 2>"$fetch_error"); then
      rm -f "$fetch_error"
    else
      curl_error=$(cat "$fetch_error")
      rm -f "$fetch_error"
      versions_raw=""
    fi
  fi

  if [[ -z "$versions_raw" ]] && command -v wget >/dev/null 2>&1; then
    fetch_error=$(mktemp)
    if versions_raw=$(wget -q -T 30 -O - "$api_url" 2>"$fetch_error"); then
      rm -f "$fetch_error"
    else
      wget_error=$(cat "$fetch_error")
      rm -f "$fetch_error"
      versions_raw=""
    fi
  fi

  if [[ -z "$versions_raw" ]]; then
    _msg "Failed to fetch remote versions:\n${curl_error:-curl unavailable or returned no details}\n${wget_error:-wget unavailable or returned no details}" --duration 5 >&2
    return 1
  fi

  if ! printf '%s\n' "$versions_raw" | jq -e '.results | type == "array"' >/dev/null 2>&1; then
    _msg "Remote versions response is invalid." --duration 5 >&2
    return 1
  fi

  printf '%s\n' "$versions_raw" >"$VERSIONS_CACHE_FILE"
  _msg "Remote versions fetched and cached" --duration 2 >&2
  echo "$versions_raw" # Return the fetched data
}

_version_current() {
  _get_version
}

_min_minor_version() {
  local min_version="${AM5_MIN_VERSION:-5.8}"
  local min_minor

  min_minor="${min_version#5.}"
  min_minor="${min_minor%%.*}"

  if [[ "$min_minor" =~ ^[0-9]+$ ]]; then
    echo "$min_minor"
  else
    echo "8"
  fi
}

_version_is_supported() {
  local name="$1"
  local min_minor="$2"
  local minor

  if [[ "$name" == "latest" ]]; then
    return 0
  fi

  if [[ ! "$name" =~ ^5\.([0-9]+)(\.[0-9]+)?(-[0-9A-Za-z][0-9A-Za-z.-]*)?$ ]]; then
    return 1
  fi

  minor="${BASH_REMATCH[1]}"
  [[ "$minor" -ge "$min_minor" ]]
}

_version_is_production() {
  local name="$1"

  [[ "$name" == "latest" || "$name" =~ ^5\.[0-9]+(\.[0-9]+)?$ ]]
}

_versions_data() {
  local versions_raw

  if [[ -e "$VERSIONS_CACHE_FILE" ]]; then
    versions_raw=$(cat "$VERSIONS_CACHE_FILE")
    if printf '%s\n' "$versions_raw" | jq -e '.results | type == "array"' >/dev/null 2>&1; then
      echo "$versions_raw"
      return 0
    fi
  else
    versions_raw=$(_fetch) || return 1
    echo "$versions_raw"
    return 0
  fi

  versions_raw=$(_fetch) || return 1
  echo "$versions_raw"
}

_select_version() {
  local mode=$1 # "production" or "all"
  local options_raw
  local version_names
  local name
  local options=()

  case "$mode" in
    production)
      if ! options_raw=$(_list_versions production); then
        _msg "Could not load versions. Use \"Update versions list\" and try again." --duration 4
        _main
        return
      fi
      ;;
    all)
      if ! options_raw=$(_list_versions all); then
        _msg "Could not load versions. Use \"Update versions list\" and try again." --duration 4
        _main
        return
      fi
      ;;
    *)
      _msg "Invalid mode: $mode" --duration 2
      _main
      return
      ;;
  esac

  mapfile -t version_names <<<"$options_raw"
  for name in "${version_names[@]}"; do
    [[ -z "$name" ]] && continue
    options+=("$name" "")
  done

  if [[ ${#options[@]} -eq 0 ]]; then
    _msg "No compatible AccessMod versions found." --duration 4
    _main
    return
  fi

  dialog \
    --backtitle "$BACKTITLE" \
    --menu "Select version:\n$HELP_NAV" "$HEIGHT" "$WIDTH" 10 \
    "${options[@]}" 2>"$TMP_FILE"

  local exit_status=$?

  if [[ "$exit_status" -ne 0 ]]; then
    _main
    return
  fi

  local selection
  selection=$(<"$TMP_FILE")

  if [[ -z "$selection" ]]; then
    _msg "No version selected." --duration 2
    _main
    return
  fi

  _update "$selection"
}

_list_versions() {
  local mode=$1
  local versions_raw
  local version_names
  local name
  local min_minor

  versions_raw=$(_versions_data) || return 1
  min_minor=$(_min_minor_version)

  if ! version_names=$(printf '%s\n' "$versions_raw" | jq -r '(.results // [])[]? | .name // empty | select(type == "string")'); then
    return 1
  fi

  while IFS= read -r name; do
    [[ -z "$name" ]] && continue
    _version_is_supported "$name" "$min_minor" || continue

    if [[ "$mode" == "production" ]] && ! _version_is_production "$name"; then
      continue
    fi

    printf '%s\n' "$name"
  done <<<"$version_names"
}

_update() {
  local ver
  ver="$1"

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
  _msg "Preparing AccessMod. This can take a few minutes..." --duration 1
  bash "$AM5_SCRIPTS_FOLDER/start.sh"
}

_remove_old_images() {
  local ver img old_images old_images_txt
  ver=$(_get_version)
  img="$AM5_REPO:$ver"

  # Safely capture the list of old image IDs into an array
  mapfile -t old_images < <(docker images -q --filter "before=$img")

  if [[ ${#old_images[@]} -eq 0 ]]; then
    _msg "No image to remove" --duration 2
    _main
    return
  fi

  old_images_txt=$(docker images --filter "before=$img")

  dialog \
    --backtitle "$BACKTITLE" \
    --clear \
    --ok-label "Confirm" \
    --no-label "Ignore" \
    --yesno "Remove old images:\n$old_images_txt" "$HEIGHT" "$WIDTH"

  if [[ $? -eq 0 ]]; then
    docker rmi "${old_images[@]}"
  fi

  _main
}

_welcome_message() {
  if _check_server_health; then
    echo -e "Welcome to AccessMod $(_version_current)\n\nThe application should be available at\nhttp://localhost:$AM5_PORT_APP_PUBLIC"
  else
    echo -e "Welcome to AccessMod $(_version_current)\n\nThe server is not running. Start/Restart it?"
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
    "2" "Update versions list" \
    "3" "Stop the server" \
    "4" "Start/Restart the server" \
    "5" "Stop the virtual machine" \
    "6" "Remove old versions" \
    "7" "Refresh status" \
    "8" "Update this manager" 2>"$TMP_FILE"

  if [[ "$?" -ne 0 ]]; then
    dialog --clear
    echo "Have a nice day! (type 'menu' to reopen the menu)"
  else
    res=$(cat "$TMP_FILE")
    case "$res" in
    0) _select_version production ;;
    1) _select_version all ;;
    2)
      _fetch >/dev/null
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
    5) _poweroff ;;
    6) _remove_old_images ;;
    7) _refresh_status ;;
    8) _update_scripts_menu ;;
    *) _main ;;
    esac
  fi
}

_main() {
  _welcome
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  if _check_server_health; then
    echo "Server OK"
  else
    _start
  fi

  _main
fi
