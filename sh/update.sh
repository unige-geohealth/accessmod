#!/bin/bash

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

        # merged by the user ! git merge FETCH_HEAD
        echo "merged updates"
        echo -e "$msgUpdateReady" >> $logPath
        
      else
        echo "Git diff HEAD FETCH_HEAD == 0 : no update"
        echo -e "$msgNoUpdate" >> "$logPath"
      fi
    else
      echo -e "$msgNoGit" >> "$logPath"
    fi
  fi
fi
