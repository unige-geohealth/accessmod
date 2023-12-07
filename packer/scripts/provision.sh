#!/bin/sh
set -e

#
# Load var env
# 
. /etc/profile

#
# Add dependencies
#
apk upgrade
apk add \
  bash \
  docker \
  virtualbox-guest-additions \
  dialog \
  util-linux \
  jq 




#
# Register init services 
#
rc-update add docker boot
rc-update add local default
service docker start

#
# Check that docker is started
#
sleep 5
if service -e docker ; then 
  echo "Docker started ok"
else 
  echo "Docker not started, provision failed"
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


echo "Move stuff"
mv /tmp/scripts $AM5_SCRIPTS_FOLDER
chown -R accessmod $AM5_SCRIPTS_FOLDER
mv $AM5_SCRIPTS_FOLDER/inittab /etc/inittab 
mv $AM5_SCRIPTS_FOLDER/profile /home/accessmod/.profile
chmod +x $AM5_SCRIPTS_FOLDER/start.sh
chmod +x $AM5_SCRIPTS_FOLDER/menu_init.sh
#CMD_START=$AM5_SCRIPTS_FOLDER/start.sh
#(crontab -l ; echo "@reboot $CMD_START") | sort - | uniq - | crontab -

# clear disk 
dd if=/dev/zero of=/fill bs=1M count=`df -m /  | tail -n1 | awk '{print $3}'` 2>/dev/null
rm /fill

# mark as ready
touch $AM5_SCRIPTS_FOLDER/ready
