#!/bin/sh
# Improved timezone setup script

# Try to get current system timezone first, if it exists
if [ -f /etc/timezone ]; then
  CURRENT_TZ=$(cat /etc/timezone)
else
  CURRENT_TZ=""
fi

# Default timezone if nothing else works
DEFAULT_TZ="UTC"

# Try to get the timezone from ipapi.co using wget
# Only if we don't already have a valid timezone
if [ -z "$CURRENT_TZ" ] || [ "$CURRENT_TZ" = "" ]; then
  echo "No current timezone set, attempting to detect timezone..."
  TIMEZONE=$(wget -qO- --timeout=5 https://ipapi.co/timezone 2>/dev/null || echo "$DEFAULT_TZ")
else
  echo "Using current system timezone: $CURRENT_TZ"
  TIMEZONE="$CURRENT_TZ"
fi

# Verify the timezone is valid
if [ ! -f "/usr/share/zoneinfo/$TIMEZONE" ]; then
  echo "Invalid timezone: $TIMEZONE, falling back to UTC"
  TIMEZONE="$DEFAULT_TZ"
fi

# Set the timezone
ln -sf /usr/share/zoneinfo/$TIMEZONE /etc/localtime
echo "$TIMEZONE" > /etc/timezone

# Export TZ for all processes to read
export TZ="$TIMEZONE"

# Add TZ to system profile so it's available for all future processes
echo "export TZ=\"$TIMEZONE\"" > /etc/profile.d/timezone.sh
chmod +x /etc/profile.d/timezone.sh

echo "Timezone set to: $TIMEZONE"

# Execute the command passed to the script
exec "$@"
