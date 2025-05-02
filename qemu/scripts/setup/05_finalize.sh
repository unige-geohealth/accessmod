#!/bin/sh

# Source common functions and variables
. /tmp/scripts/setup/common.sh

log "Starting finalization..."

# Check required variables
check_required_vars "AM5_SCRIPTS_FOLDER"

# Clear disk space
log "Clearing disk space..."
dd if=/dev/zero of=/fill bs=1M count=$(df -m / | tail -n1 | awk '{print $3}') 2>/dev/null || true
rm -f /fill

# Mark as ready
log "Marking system as ready..."
touch ${AM5_SCRIPTS_FOLDER}/ready

# Clean up setup scripts
log "Cleaning up setup files..."
rm -rf /tmp/scripts/setup

log "Finalization completed"
log "AccessMod VM setup completed successfully"
