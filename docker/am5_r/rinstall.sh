#! /usr/bin/env sh

pkg=$1
action=$2
cmd=""

if [[ "$action" = "remove" ]];
then
  cmd="\
    tryCatch({
    if(require('"$pkg"')) remove.packages('"$pkg"')
  },error=stop)";
elif [[ "$action" = "github" ]];
then
  cmd="\
    tryCatch({ 
      pkg=strsplit('"$pkg"','/')[[1]][2]
      if(!require('remotes')){
        install.packages('remotes', repos='"$R_REPO"')
      }
      remotes::install_github('"$pkg"')
      if(!require(pkg, character.only=TRUE)){
        stop('"$pkg" not installed')
      }
  },error = stop)";
else
  cmd="\
    tryCatch({
    install.packages('"$pkg"', Ncpus=$R_NCPU, repos='"$R_REPO"')
    if(!require('"$pkg"', character.only=TRUE)){
      stop('"$pkg"not installed')
    }
  },error=stop)";
fi;

#echo "Install command: "$cmd;

R -q -e "$cmd";


