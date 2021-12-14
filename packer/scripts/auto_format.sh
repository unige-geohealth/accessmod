
#
# Mount shared folder (slow) 
# -> data volume (true if fail)
#
# mount -t vboxsf dbgrass /home/accessmod/data/dbgrass || true 


#
# Auto format sdb if exactly one line found ( exists, but no partition )
#
#if [ $(lsblk -ln | grep '^sdb' | wc -l) -eq 1 ]; then
  #parted /dev/sdb mklabel gpt
  #parted -a opt /dev/sdb mkpart primary ext4 0% 100%
  #mkfs.ext4 -L accessmod_data /dev/sdb1
#fi

#
# Mount external drive, if found 
#
#SDB1=$(df | grep '/dev/sdb1')
#SDB1_MOUNT_POINT=$(echo $SDB1 | awk '{ print $6 }')
#if [ -n "$SDB1" ]; then
  #if [ -z "$SDB1_MOUNT_POINT" ]; then
    #mkdir -p $DATAPATH_SDB
    #mount -o defaults /dev/sdb1 $DATAPATH_SDB 
    #DATAPATH=$DATAPATH_SDB
  #fi
#fi
#-v $DATAPATH:/data \


