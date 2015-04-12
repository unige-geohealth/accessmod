#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# amUI.R :
# additional function for UI elements.

# load external ui file
loadUi<-function(path){
  source(path)[1]
}


# new file input
amFileInput<-function (inputId, label, fileAccept=NULL, multiple=FALSE){
  inputTag<-tags$input(
    type='file',
    class='upload',
    accept=paste(fileAccept,collapse=','),
    id=inputId,
    name=inputId)
  if(multiple) inputTag$attribs$multiple='multiple'
  spanTag<-tags$span(label)
  inputClass<-tags$button(
    class=c('btn-browse btn btn-default'),
    id=inputId,
    tList<- tagList(
      spanTag,
      inputTag
      )
    )
  tagList(inputClass,
    tags$div(id = paste(inputId,"_progress", sep = ""), 
      class = "progress progress-striped active shiny-file-input-progress",
      tags$div(class = "progress-bar"), tags$label()))
}

# simple panel extending box
amPanel<-function(...,width=9){
  tags$div(class=paste0('col-sm-',as.integer(width)),
    tags$div(class='box box-solid no-padding am-square',
      tags$div(class='box-body',
        ...
        )
      ))
}


# add dependencies to an existing shiny function
addUIDep <- function(x) {
  jqueryUIDep <- htmlDependency("jqueryui", "1.10.4", c(href="shared/jqueryui/1.10.4"),
    script = "jquery-ui.min.js",
    stylesheet = "jquery-ui.min.css")

  attachDependencies(x, c(htmlDependencies(x), list(jqueryUIDep)))
}

# amProgressBar  : display a progressbar
# idBar : div id
amProgressBar<-function(idBar=""){
  div(class="am-progress-container",
    tags$div(id=idBar,class='am-progress-bar',style="width:0%")
    )
}


# function to create accordion in UI
#id= unique html ID
# itemList = list of named list with title and content element. See example.
amAccordionGroup<-function(id,show=NULL,itemList){
  cnt=0
  contentList<-lapply(itemList,function(x){
    cnt<<-cnt+1
    ref<-paste0(amSubPunct(id,'_'),cnt)
    showItem<-ifelse(cnt %in% show,'collapse in','collapse')
    stopifnot(!is.list(x) || !is.null(x$title) || !char(x$title)<1 || !is.null(x$content) || !nchar(x$content)<1)
    div(class="panel panel-default",
      div(class="panel-heading",
        h4(class="panel-title",
          a('data-toggle'="collapse", 'data-parent'=paste0('#',id),href=paste0("#",ref),x$title)
          )
        ),
      div(id=ref,class=paste("panel-collapse",showItem),
        div(class="panel-body",x$content)
        )
      )
})

  return(div(class="panel-group",id=id,
      contentList
      ))
}
# example:
#amAccordionGroup(id='superTest',
#  itemList=list(
#    'a'=list('title'='superTitle',content='acontent'),
#    'b'=list('title'='bTitle',content='bContent'))
#  )
#


aQ<-quote
aH<-function(amId,level,content){
  dbCon=configHelp$dbCon
  module=configHelp$module
  stopifnot(is.language(content),!is.null(dbCon),is.numeric(level),!is.null(module),!is.null(amId))

  amIdHelp=paste0('am-help-',amId)
  tableExists<-isTRUE(dbExistsTable(dbCon,'amHelp'))
  keyExists=FALSE

  if(tableExists){ 
    keyExists <-isTRUE(amIdHelp %in% dbGetQuery(dbCon,"SELECT amId FROM amHelp")$amId)
  }

  if(!tableExists | !keyExists){
    helpEntry<-data.frame(amId=amIdHelp,module=module,level=level,title="",help="")
    dbWriteTable(dbCon,"amHelp",helpEntry,row.names = F,append=tableExists)
  }

  div(class='am-help',id=amId,eval(content))
}
