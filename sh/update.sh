#!/bin/bash

hostname="accessmod"
os="Linux"
gitHost="github.io"
logPath="/srv/shiny-server/logs/logs.txt"



if [ "`hostname`" == "$hostname" -a "`uname`" == "$os" ]
then
  if [ ! -e "$logPath" ]
  then
    echo "$logPath not found"
  else
    gitPing=`ping -c1 $gitHost`
    gitOk=`echo $gitPing | grep "\s0% packet loss"  | wc -l`

    msgNoGit=`date +"%Y-%m-%d"`" \t warning \t $gitHost not reachable. $gitPing "
    msgNoUpdate=`date +"%Y-%m-%d"`" \t log \t No update. "
    msgUpdateDone=`date +"%Y-%m-%d"`" \t log \t Update done "

    if [ "$gitOk" -eq 1 ]
    then
      # fetch changes, git stores them in FETCH_HEAD
      git fetch

      # check for remote changes in origin repository
      newUpdatesAvailable=`git diff HEAD FETCH_HEAD`
      if [ "$newUpdatesAvailable" != "" ]
      then
        # create the fallback
        git branch fallbacks
        git checkout fallbacks

        git add .
        git add -u
        git commit -m `date "+%Y-%m-%d"`
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
