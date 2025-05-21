#!/bin/bash

# Source required scripts
. "$AM5_SCRIPTS_FOLDER/env.sh"
. "$AM5_SCRIPTS_FOLDER/message.sh"
. "$AM5_SCRIPTS_FOLDER/helpers.sh"

# Function to update scripts from GitHub
_update_scripts() {
  local branch="${1:-main}"
  local temp_dir="/tmp/accessmod_scripts"
  local scripts_dir="$AM5_SCRIPTS_FOLDER"
  local target_path="qemu/fs/home/accessmod/scripts"

  _msg "Updating scripts from branch: $branch..." --duration 2
  
  # Create temp directory (clean if exists)
  rm -rf "$temp_dir"
  mkdir -p "$temp_dir"
  
  # Download scripts from GitHub
  if ! wget -q -O "$temp_dir/scripts.zip" "$AM5_GITHUB_REPO/archive/refs/heads/$branch.zip"; then
    _msg "Failed to download scripts from branch: $branch" --duration 3
    rm -rf "$temp_dir"
    _main
    return 1
  fi
  
  # Extract only the scripts directory
  if ! unzip -q -o "$temp_dir/scripts.zip" "accessmod-$branch/$target_path/*" -d "$temp_dir"; then
    _msg "Failed to extract scripts from archive" --duration 3
    rm -rf "$temp_dir"
    _main
    return 1
  fi
  
  # Check if the extracted directory exists
  if [ ! -d "$temp_dir/accessmod-$branch/$target_path" ]; then
    _msg "Scripts directory not found in downloaded archive" --duration 3
    rm -rf "$temp_dir"
    _main
    return 1
  fi
  
  # Copy scripts to the scripts directory
  cp -f "$temp_dir/accessmod-$branch/$target_path/"* "$scripts_dir/"
  chmod +x "$scripts_dir/"*.sh
  
  # Clean up
  rm -rf "$temp_dir"
  
  _msg "Scripts updated successfully. Restarting menu..." --duration 3
  
  # Restart the menu
  exec "$scripts_dir/menu_init.sh"
}

# Function to show update menu
_update_scripts_menu() {
  dialog \
    --backtitle "$BACKTITLE" \
    --clear \
    --ok-label "Update" \
    --cancel-label "Cancel" \
    --menu "Select branch to update from:\n$HELP_NAV" "$HEIGHT" "$WIDTH" 10 \
    "main" "Stable version" \
    "staging" "Development version" 2>"$TMP_FILE"
  
  if [[ "$?" -ne 0 ]]; then
    _main
    return
  fi
  
  local branch
  branch=$(<"$TMP_FILE")
  
  dialog \
    --backtitle "$BACKTITLE" \
    --clear \
    --ok-label "Confirm" \
    --no-label "Cancel" \
    --yesno "Confirm updating scripts from branch: $branch?" "$HEIGHT" "$WIDTH"
  
  if [[ "$?" -ne 0 ]]; then
    _main
    return
  fi
  
  _update_scripts "$branch"
}
