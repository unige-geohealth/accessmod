# functions to handle update

amUpdateApp<-function(force=FALSE){
  defMsg = ams("update_do_not_reload")
  
  progressBarControl(
    visible=TRUE,
    percent=0,
    title=defMsg,
    text=ams("update_merge_code")
    )
  
  if(force){
    system('git reset --hard HEAD && git pull')
  }else{
    system('git merge FETCH_HEAD')
  }

  if(file.exists('.fetched_version')){
    file.remove('.fetched_version')
  }
  if(file.exists('.fetched_changes')){
    file.remove('.fetched_changes')
  }

  progressBarControl(
    visible=TRUE,
    percent=30,
    title=defMsg,
    text=ams("update_install_libraries"),
    timeOut=2
    )

  system("Rscript global.R")

  progressBarControl(
    visible=TRUE,
    percent=90,
    title=defMsg,
    text=ams("update_restart"),
    timeOut=2
    )

  amRestart()
}

amGetAppRevisionCurrent <-function(){
  system("git rev-parse --verify HEAD | awk '{print substr($0,1,7)}'",intern=T)
}

amGetAppRevisionFetched<-function(){
  fetched <- system("git rev-parse --verify FETCH_HEAD | awk '{print substr($0,1,7)}'",intern=T)
  if(amNoDataCheck(fetched)) fetched <- amGetAppRevisionCurrent() 
  fetched
}

amGetAppCurrentBranch<-function(){
  current <- system("git branch | grep '*' |awk '{ print $2}'",intern=T)
  if(amNoDataCheck(current)) current = "devel"
  current
}

amGetAppVersionCurrent <- function(){
  readLines("version.txt")
}

amHasAppUpdate <- function(){
  isTRUE(file.exists('.fetched_version'))
}

amGetAppVersionFetched <- function(){
  if(amHasAppUpdate()){
    readLines('.fetched_version')
  }
}
amGetAppChangesFetched <- function(){
  changes <- ""
  if(amHasAppUpdate()){
    changes <- readLines(".fetched_changes") %>%
    paste( collapse="\n")
  }
  changes
}
amGetAppChangesCurrent<- function(){
  readLines("changes.md") %>% paste(collapse='\n')
}

amUiLinkRepoRelease <- function(
  version = amGetAppVersionCurrent(),
  text = amGetAppVersionCurrent()
  ){
  tags$a(
    target = "_blank",
    href = paste0(config$repository,'/releases/tag/',version),
    text
    )
}

amGetAppVersionCurrentLink <- function(){
 amUiLinkRepoRelease(
    version =  amGetAppVersionCurrent(),
    text = amGetAppVersionCurrent()
   )
}
amGetAppVersionFetchedLink <- function(){
  #if(amHasAppUpdate()){
    amUiLinkRepoRelease(
      version =  amGetAppVersionFetched(),
      text = amGetAppVersionFetched()
      )
  #}
}



