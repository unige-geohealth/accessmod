#!/bin/sh

# Source common functions and variables
. /tmp/scripts/setup/common.sh

log "Starting user setup..."

# Check required variables
check_required_vars "USERNAME" "USERPASSWORD"

# Create user and configure sudo
log "Creating user ${USERNAME}..."
adduser ${USERNAME} -D -G wheel
addgroup ${USERNAME} docker

log "Setting user password..."
echo "${USERNAME}:${USERPASSWORD}" | chpasswd

log "Configuring sudo access..."
echo "%wheel ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/wheel
chmod 0440 /etc/sudoers.d/wheel

# Create scripts directory and set permissions
log "Setting up scripts directory..."
mkdir -p ${AM5_SCRIPTS_FOLDER}
cp -r /tmp/scripts/* ${AM5_SCRIPTS_FOLDER}/
rm -f ${AM5_SCRIPTS_FOLDER}/inittab  # Remove inittab since we create it in system setup
rm -rf ${AM5_SCRIPTS_FOLDER}/setup   # Remove setup scripts as they're not needed in the VM
chown -R ${USERNAME} ${AM5_SCRIPTS_FOLDER}
chmod +x ${AM5_SCRIPTS_FOLDER}/*.sh

log "User setup completed"
