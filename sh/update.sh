



hostname="accessmod"
gitOk=`ping -c1 github.io | grep "\s0% packet loss" | wc -l`
os="Linux"

if [ `hostname` == $hostname  && `uname` == $os && gitOk -eq 1 ]
  
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
    git merge FETCH_HEAD
    echo "merged updates"
    touch restart.txt
  else
    echo "no updates available"
  fi
