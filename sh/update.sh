#!/bin/bash

dateStamp=`date "+%Y-%m-%d@%H_%M_%S"`
hostname="accessmod"
dirCurrent=basename`pwd`
dirTest="accessmod_latest"
os="Linux"
gitHost="github.io"
logPath="/srv/shiny-server/logs/logs.txt"
name="shiny accessmod"
email="f@fxi.io"


if [ "`hostname`" == "$hostname" -a "`uname`" == "$os" ]
then
  if [ ! -e "$logPath" ]
  then
    echo "$logPath not found"
  else
    gitPing=`ping -c1 $gitHost`
    gitOk=`echo $gitPing | grep "\s0% packet loss"  | wc -l`

    msgNoGit=$dateStamp"\twarning\t$gitHost not reachable."
    msgNoUpdate=$dateStamp"\tlog\tNo update."
    msgUpdateDone=$dateStamp"\tlog\tUpdate done"

    if [ "$gitOk" -eq 1 ]
    then

       if [ "`git config --get user.name`" == "" ]
       then
         git config --global user.name $name
         git config --global user.email $email
       fi

       # get current branch
      currentBranch=$(git branch | grep '*' |awk '{ print $2}')
      # fetch changes, git stores them in FETCH_HEAD

      git fetch origin $currentBranch

      # check for remote changes in origin repository
      newUpdatesAvailable=`git diff HEAD FETCH_HEAD`
      if [ "${#newUpdatesAvailable}" -gt 0 ]
      then
       
        # create the fallback
        git checkout -B fallbacks

        git add .
        git add -u
        git commit -m $dateStamp
        echo "fallback created"

        git checkout $currentBranch

        # merged by the user ! git merge FETCH_HEAD
        echo "merged updates"
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
