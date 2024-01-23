#!/bin/sh
set -e

#
# Set motd
#
echo 'Welcome to AccessMod Alpine' > /etc/motd

#
# Environment variables for AccessMod
#
echo "export AM5_PORT_APP=$AM5_PORT_APP" >> /etc/profile
echo "export AM5_PORT_APP_PUBLIC=$AM5_PORT_APP_PUBLIC" >> /etc/profile
echo "export AM5_PORT_HTTP=$AM5_PORT_HTTP" >> /etc/profile
echo "export AM5_PORT_HTTP_PUBLIC=$AM5_PORT_HTTP_PUBLIC" >> /etc/profile
echo "export AM5_ARCHIVE_DOCKER=$AM5_ARCHIVE_DOCKER" >> /etc/profile
echo "export AM5_SCRIPTS_FOLDER=$AM5_SCRIPTS_FOLDER" >> /etc/profile
echo "export AM5_VERSION_FILE=$AM5_VERSION_FILE" >> /etc/profile
echo "export AM5_VERSION=$AM5_VERSION" >> /etc/profile
echo "export AM5_REPO=$AM5_REPO" >> /etc/profile
echo "export AM5_HUB_API=$AM5_HUB_API" >> /etc/profile
echo "export AM5_IMAGE=$AM5_IMAGE" >> /etc/profile

#
# Install dependencies
# 
echo "$ALPINE_REPOSITORY" >> /etc/apk/repositories

apk update
apk add \
  sudo \
  bash \
  docker \
  virtualbox-guest-additions \
  dialog \
  util-linux \
  jq 

#
# Set users 
#
echo '%wheel ALL=(ALL) NOPASSWD:ALL' > /etc/sudoers.d/wheel
adduser $SSH_USERNAME --disabled-password
adduser $SSH_USERNAME wheel
echo "$SSH_USERNAME:$SSH_PASSWORD" | chpasswd


#
# SSH 
# configured in alpine_setup
#
echo 'PasswordAuthentication yes' >> /etc/ssh/sshd_config
echo 'PubkeyAuthentication yes' >> /etc/ssh/sshd_config

#
# Register init services 
#
rc-update add docker boot
rc-update add local default
rc-update add sshd
service sshd start
service docker start

#
# Check that Docker is started
#
max_attempts=10
attempt_num=1

while [ $attempt_num -le $max_attempts ]; do
    echo "Checking if Docker is running (Attempt $attempt_num/$max_attempts)..."

    # Check if the Docker service is active
    if service docker status > /dev/null 2>&1; then
        echo "Docker started ok."
        break
    fi

    # Increment the attempt number
    attempt_num=$((attempt_num+1))

    # Wait for 1 second before retrying
    sleep 1
done

# If Docker is not running after the attempts, exit with an error
if [ $attempt_num -gt $max_attempts ]; then
    echo "Docker not started, provision failed."
    exit 1
fi


#
#  Create volumes
#
docker volume create am_data_cache
docker volume create am_data_logs
docker volume create am_data_grass


#
# Create an image archive to compress
#
docker pull $AM5_IMAGE
sudo docker save $AM5_IMAGE | gzip >  $AM5_ARCHIVE_DOCKER
sudo docker rmi $AM5_IMAGE
sudo docker system prune -af

#
# Save version 
#
echo $AM5_VERSION > $AM5_VERSION_FILE
chown accessmod $AM5_VERSION_FILE
echo "Save alias for menu"
echo "alias menu='sh $AM5_SCRIPTS_FOLDER/menu_init.sh'" >> /etc/profile


#
# Move imported scripts and settings 
#
echo "Move data"
mv /tmp/scripts $AM5_SCRIPTS_FOLDER
chown -R accessmod $AM5_SCRIPTS_FOLDER
mv $AM5_SCRIPTS_FOLDER/inittab /etc/inittab 
mv $AM5_SCRIPTS_FOLDER/profile /home/accessmod/.profile
chmod +x $AM5_SCRIPTS_FOLDER/start.sh
chmod +x $AM5_SCRIPTS_FOLDER/menu_init.sh

# clear disk 
dd if=/dev/zero of=/fill bs=1M count=`df -m /  | tail -n1 | awk '{print $3}'` 2>/dev/null
rm /fill

# mark as ready
touch $AM5_SCRIPTS_FOLDER/ready

exit 0


