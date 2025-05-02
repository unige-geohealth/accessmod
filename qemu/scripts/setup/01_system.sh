#!/bin/sh

# Source common functions and variables
. /tmp/scripts/setup/common.sh

log "Starting system setup..."

# Check required variables
check_required_vars "ALPINE_REPO"

# Add community repository
log "Adding community repository..."
echo "${ALPINE_REPO}" >> /etc/apk/repositories

# Set up auto-login
log "Configuring auto-login..."
cat > /etc/inittab << EOF
# /etc/inittab

::sysinit:/sbin/openrc sysinit
::sysinit:/sbin/openrc boot
::wait:/sbin/openrc default

tty1::respawn:/sbin/agetty --autologin ${USERNAME} --noclear 38400 tty1 linux
tty2::respawn:/sbin/agetty 38400 tty2 linux
tty3::respawn:/sbin/agetty 38400 tty3 linux
tty4::respawn:/sbin/agetty 38400 tty4 linux
tty5::respawn:/sbin/agetty 38400 tty5 linux
tty6::respawn:/sbin/agetty 38400 tty6 linux

# Stuff to do for the 3-finger salute
::ctrlaltdel:/sbin/reboot

# Stuff to do before rebooting
::shutdown:/sbin/openrc shutdown
EOF

# Set motd
log "Setting up message of the day..."
echo 'Welcome to AccessMod Alpine' > /etc/motd

log "System setup completed"
