#!/bin/bash

# _msg: Display messages in interactive or non-interactive mode with named parameters.
# Usage: _msg "Message" [--duration <duration>] [--title <title>] [--backtitle <backtitle>] [--interactive <interactive>]
#   "Message" - Text to display.
#   --duration <duration> - Time to show in interactive mode, defaults to 2 seconds.
#   --title <title> - Dialog box title, optional.
#   --backtitle <backtitle> - Dialog box back title, optional.
#   --interactive <interactive> - If set, uses `dialog`; otherwise, prints to stdout.

_msg() {
  local message="$1"
  shift
  local duration=2
  local title="$TITLE"
  local backtitle="$BACKTITLE"
  local interactive="$INTERACTIVE"

  while [[ $# -gt 0 ]]; do
    case "$1" in
    --duration)
      duration="$2"
      shift 2
      ;;
    --title)
      title="$2"
      shift 2
      ;;
    --backtitle)
      backtitle="$2"
      shift 2
      ;;
    --interactive)
      interactive="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1" >&2
      return 1
      ;;
    esac
  done

  if [[ "$interactive" -ne 0 ]]; then
    dialog --backtitle "$backtitle" --title "$title" \
      --infobox "$message" 3 70
    sleep "$duration"
  else
    echo "$message"
  fi
}
