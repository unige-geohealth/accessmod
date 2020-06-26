#!/bin/bash
#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#    
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.


dateStamp=`date "+%Y-%m-%d@%H_%M_%S"`
hostname="accessmod"
dirCurrent=basename`pwd`
dirTest="accessmod_latest"
origin="origin"
os="Linux"
gitHost="github.com"
appPath="/srv/shiny-server/accessmod"
appPathTesting="/srv/shiny-server/accessmod_testing"
logPath="/srv/shiny-server/logs/logs.txt"
name="shiny accessmod"
email="f@fxi.io"
user="shiny"
pathNow=`pwd`

if [ $pathNow != $appPath ] && [ $pathNow != $appPathTesting ]
then 
  echo 'current dir is '`pwd`', cd '$appPath
  cd $appPath
else
  echo 'current dir is '`pwd`', we can check for update '
fi

if [ `whoami` != $user ]
then 
  echo "This script should be run as "$user
  exit 1
else
  echo "Start update script as "$user
fi

if [ "`hostname`" == "$hostname" -a "`uname`" == "$os" ]
then
  if [ ! -e "$logPath" ]
  then
    echo "$logPath not found"
  else
    gitRevListCount=`git ls-remote --tags $origin | wc -l`

    msgNoGit=$dateStamp"\twarning\t$gitHost not reachable."
    msgNoUpdate=$dateStamp"\tlog\tNo update."
    msgUpdateReady=$dateStamp"\tlog\tUpdate ready to be installed"

    if [ "$gitRevListCount" -gt 0 ]
    then

       if [ "`git config --get user.name`" == "" ]
       then
         echo "Config git as name "$name" and email "$email
         git config --global user.name $name
         git config --global user.email $email
       fi

       # get current branch
      currentBranch=$(git branch | grep '*' |awk '{ print $2}')
      # fetch changes, git stores them in FETCH_HEAD

      echo "Fetch branch "$currentBranch
      git fetch $origin $currentBranch

      # check for remote changes in origin repository
      newUpdatesAvailable=`git diff HEAD FETCH_HEAD`
 
      if [ "${#newUpdatesAvailable}" -gt 0 ]
      then
      
        echo "Git diff HEAD FETCH_HEAD > 0 : update available"
        echo "Create fallback"
        # create the fallback
        git checkout -B fallbacks
        git add .
        git add -u
        git commit -m $dateStamp
        git checkout $currentBranch
        echo "Fallback created"
       
        git checkout FETCH_HEAD
        if [ -f "version.txt" ]; then
          cat version.txt > .fetched_version
        fi
        if [ -f "changes.md" ]; then
          cat changes.md > .fetched_changes
        fi
        git checkout $currentBranch
      
        #
        # merged by the user ! git merge FETCH_HEAD
        #
        echo -e "$msgUpdateReady" >> $logPath
        
      else
        echo "Git diff HEAD FETCH_HEAD == 0 : no update"
        rm .fetched_changes 
        rm .fetched_version
        echo -e "$msgNoUpdate" >> "$logPath"
      fi

    else
      echo -e "$msgNoGit" >> "$logPath"
    fi
  fi
fi
