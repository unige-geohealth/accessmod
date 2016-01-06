#!/bin/bash

hostname="accessmod"
os="Linux"
gitHost="github.io"
logPath="/srv/shiny-server/logs/logs.txt"
dateStamp=`date "+%Y-%m-%d@%H_%M_%S"`

if [ "`hostname`" == "$hostname" -a "`uname`" == "$os" ]
then
  if [ ! -e "$logPath" ]
  then
    echo "$logPath not found"
  else
    gitPing=`ping -c1 $gitHost`
    gitOk=`echo $gitPing | grep "\s0% packet loss"  | wc -l`

    msgNoGit=$dateStamp" \t warning \t $gitHost not reachable. $gitPing "
    msgNoUpdate=$dateStamp" \t log \t No update. "
    msgUpdateDone=$dateStamp" \t log \t Update done "

    if [ "$gitOk" -eq 1 ]
    then
      # fetch changes, git stores them in FETCH_HEAD
      git fetch

      # check for remote changes in origin repository
      newUpdatesAvailable=`git diff HEAD FETCH_HEAD`
      if [ "$newUpdatesAvailable" != "" ]
      then
        # create the fallback
        git checkout -B fallbacks

        git add .
        git add -u
        git commit -m $dateStamp
        echo "fallback created"

        git checkout master

        # merged by the user ! git merge FETCH_HEAD
        echo "merged updates"
        touch restart.txt
        echo -e "$msgUpdateDone" >> $logPath
      else
        echo "no updates available"
        echo -e "$msgNoUpdate" >> "$logPath"
      fi
    else
      echo -e "$msgNoGit" >> "$logPath"
    fi
  fi
fi
