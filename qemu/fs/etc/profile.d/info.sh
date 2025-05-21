#!/bin/bash 

HOSTNAME=$(hostname)
DISTRO=$(awk -F= '$1=="PRETTY_NAME" { print $2 ;}' /etc/os-release | tr -d '"')
KERNEL=$(uname -r)
VERSION=$(awk -F= '$1=="VERSION_ID" { print $2 ;}' /etc/os-release)
CPULOAD=$(cat /proc/loadavg | awk '{print $1 ", " $2 ", " $3}')
CPUINFO=$(cat /proc/cpuinfo | grep 'model name' | head  -1 | cut -d':' -f2)
MEM=$(free -m | head -n 2 | tail -n 1 | awk {'print  $2'})
MEMFREE=$(free -m | head -n 2 | tail -n 1 | awk {'print $4'})
SWAPFREE=$(free -m | tail -n 1 | awk {'print $4'})
DISKFREE=$(df -h / | awk '{ a =  $4 } END { print a }')

cat > /etc/issue << EOF
%++++++++++++++++++++++++++ ACCESSMOD SERVER ++++++++++++++++++++++++++++++++%
%                                                                            %
    AccessMod $AM5_VERSION http://localhost:$AM5_PORT_APP_PUBLIC
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

%                                                                            %
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++%
EOF
