#!/bin/sh
set -e

#
# Set motd
#
echo 'Welcome to AccessMod Alpine' > /etc/motd

#
# Install dependencies
# - sudo required for poweroff
# - rsync for transfering/sync data
# 
echo "$ALPINE_REPOSITORY" >> /etc/apk/repositories

apk --no-cache add \
  bash \
  docker \
  virtualbox-guest-additions \
  dialog \
  jq \
  util-linux \
  sudo \
  rsync 

#
# Set users 
#
adduser $USERNAME -D -G wheel
addgroup $USERNAME docker
echo "Change password $USERNAME:$PASSWORD"
echo "$USERNAME:$PASSWORD" | chpasswd
echo "%wheel ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/wheel
chmod 0440 /etc/sudoers.d/wheel

#
# SSH 
#
echo 'PasswordAuthentication yes' >> /etc/ssh/sshd_config
echo 'PubkeyAuthentication yes' >> /etc/ssh/sshd_config

#
# Register init services 
#
rc-update add docker boot
rc-update add local default
rc-update add sshd
service docker start
sleep 10


#
#  Create volumes
#
docker volume create am_data_cache
docker volume create am_data_logs
docker volume create am_data_grass


#
# Pull base image
#
docker pull $AM5_REPO:$AM5_VERSION_LATEST
AM5_VERSION=$(docker run -rm --ti $AM5_REPO:$AM5_VERSION_LATEST cat version.txt)

#
# Save version 
#
echo $AM5_VERSION > $AM5_VERSION_FILE
chown accessmod $AM5_VERSION_FILE

#
# Move imported scripts and settings 
#
mv /tmp/scripts $AM5_SCRIPTS_FOLDER
chown -R accessmod $AM5_SCRIPTS_FOLDER
mv $AM5_SCRIPTS_FOLDER/inittab /etc/inittab 
chmod +x $AM5_SCRIPTS_FOLDER/start.sh
chmod +x $AM5_SCRIPTS_FOLDER/menu_init.sh

#
# Set profile
#  - Create a startup script 
#  - Export usefull variables 
#  - Create an alias 
#  - launch the menu 
#
ENV_SCRIPT=/etc/profile.d/am5_env.sh
echo "#!/bin/bash" > $ENV_SCRIPT
echo "export AM5_NAME=accessmod" >> $ENV_SCRIPT
echo "export AM5_VERSION=$AM5_VERSION" >> $ENV_SCRIPT
echo "export AM5_PORT_APP=$AM5_PORT_APP" >> $ENV_SCRIPT
echo "export AM5_PORT_APP_PUBLIC=$AM5_PORT_APP_PUBLIC" >> $ENV_SCRIPT
echo "export AM5_PORT_HTTP=$AM5_PORT_HTTP" >> $ENV_SCRIPT
echo "export AM5_PORT_HTTP_PUBLIC=$AM5_PORT_HTTP_PUBLIC" >> $ENV_SCRIPT
echo "export AM5_SCRIPTS_FOLDER=$AM5_SCRIPTS_FOLDER" >> $ENV_SCRIPT
echo "export AM5_VERSION_FILE=$AM5_VERSION_FILE" >> $ENV_SCRIPT
echo "export AM5_VERSION_LATEST=$AM5_VERSION_LATEST" >> $ENV_SCRIPT
echo "export AM5_REPO=$AM5_REPO" >> $ENV_SCRIPT
echo "export AM5_HUB_API=$AM5_HUB_API" >> $ENV_SCRIPT
echo "export AM5_MIN_VERSION=$AM5_MIN_VERSION" >> $ENV_SCRIPT
echo "alias menu='sh $AM5_SCRIPTS_FOLDER/menu_init.sh'" >> $ENV_SCRIPT
echo "menu" >> $ENV_SCRIPT
chmod +x $ENV_SCRIPT


# clear disk 
dd if=/dev/zero of=/fill bs=1M count=`df -m /  | tail -n1 | awk '{print $3}'` 2>/dev/null
rm /fill

# mark as ready
touch $AM5_SCRIPTS_FOLDER/ready

echo "AccessMod 5 provisioning finished"
exit 0

