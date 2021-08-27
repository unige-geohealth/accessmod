#!/bin/sh 

# source environment_vars
. /etc/profile

if [ -z "$AM5_PORT_EXPOSED" ]; then
 AM5_PORT_EXPOSED=8080
fi


cat > /etc/issue << EOF
%++++++++++++++++++++++++++ ACCESSMOD SERVER ++++++++++++++++++++++++++++++++%
%                                                                            %
        AccessMod available at http://localhost:$AM5_PORT_EXPOSED
%                                                                            %
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++%
%                                                                            %
        Name: `hostname`
        CPU: `cat /proc/cpuinfo | grep 'model name' | head  -1 | cut -d':' -f2`
        Memory: `free -m | head -n 2 | tail -n 1 | awk {'print  $2'}`M
        Swap: `free -m | tail -n 1 | awk {'print $2'}`M Disk: `df -h / | awk  '{ a = $2 } END { print a }'`

        Kernel: `uname -r`
        Distro: `awk -F= '$1=="PRETTY_NAME" { print $2 ;}' /etc/os-release | tr -d '"'`
        Version `awk -F= '$1=="VERSION_ID" { print $2 ;}' /etc/os-release`

        CPU Load: `cat /proc/loadavg | awk '{print $1 ", " $2 ", " $3}'`
        Free Memory: `free -m | head -n 2 | tail -n 1 | awk {'print $4'}`M
        Free Swap: `free -m | tail -n 1 | awk {'print $4'}`M
        Free Disk: `df -h / | awk '{ a =  $2 } END { print a }'`

        eth0 Address: `ifconfig eth0 | grep "inet addr" |  awk -F: '{print $2}' | awk '{print $1}'`
%                                                                            %
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++%
EOF

