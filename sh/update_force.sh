#
# Quick and dirty pull without any check. 
#

appPath="/srv/shiny-server/accessmod"
user="shiny"

sudo su $user
cd $appPath
git pull

