#!/bin/sh 

# source environment_vars
. /etc/profile

HOSTAME=$(hostname)
DISTRO=$(awk -F= '$1=="PRETTY_NAME" { print $2 ;}' /etc/os-release | tr -d '"')
KERNEL=$(uname -r)
VERSION=$(awk -F= '$1=="VERSION_ID" { print $2 ;}' /etc/os-release)
CPULOAD=$(cat /proc/loadavg | awk '{print $1 ", " $2 ", " $3}')
CPUINFO=$(cat /proc/cpuinfo | grep 'model name' | head  -1 | cut -d':' -f2)
MEM=$(free -m | head -n 2 | tail -n 1 | awk {'print  $2'})
MEMFREE=$(free -m | head -n 2 | tail -n 1 | awk {'print $4'})
SWAPFREE=$(free -m | tail -n 1 | awk {'print $4'})
DISKFREE=$(df -h / | awk '{ a =  $4 } END { print a }')
DISKFREEAM5=$(df -h /home/accessmod/data/dbgrass | awk '{ a =  $4 } END { print a }')

cat > /etc/issue << EOF
%++++++++++++++++++++++++++ ACCESSMOD SERVER ++++++++++++++++++++++++++++++++%
%                                                                            %
        AccessMod available at http://localhost:$AM5_PORT_APP_PUBLIC
%                                                                            %
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++%
%                                                                            %
        Name: $HOSTNAME 
        CPU: $CPUINFO 
        Memory: $MEM M

        Kernel: $KERNEL
        Distro: $DISTRO
        Version: $VERSION 

        CPU Load: $CPULOAD
        Free Memory: $MEMFREE M
        Free Swap: $SWAPFREE M
        Free Disk System: $DISKFREE 
        Free Disk Database: $DISKFREEAM5

%                                                                            %
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++%
EOF


