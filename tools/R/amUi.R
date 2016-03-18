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
  source(path,local=TRUE)$value
}


## new file input
#amFileInput<-function (inputId, label, fileAccept=NULL, multiple=FALSE){
#  inputTag<-tags$input(
#    type='file',
#    class='upload',
#    accept=paste(fileAccept,collapse=','),
#    id=inputId,
#    name=inputId)
#  if(multiple) inputTag$attribs$multiple='multiple'
#  spanTag<-tags$span(label)
#  inputClass<-tags$button(
#    class=c('btn-browse btn btn-default'),
#    id=inputId,
#    tList<- tagList(
#      spanTag,
#      inputTag
#      )
#    )
#  tagList(inputClass,
#    tags$div(id = paste(inputId,"_progress", sep = ""), 
#      class = "progress progress-striped active shiny-file-input-progress",
#      tags$div(class = "progress-bar"), tags$label()))
#}
#

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
  inputClass<-tags$label(
    class=c('btn-browse btn btn-default'),
    id=inputId,
    spanTag,
    inputTag
    )
  tagList(inputClass,
    tags$div(id = paste(inputId,"_progress", sep = ""), 
      class = "progress progress-striped active shiny-file-input-progress",
      tags$div(class = "progress-bar"), tags$label()))
}

#
## simple panel extending box
amPanel<-function(...,width=9){
  tags$div(class=paste0('col-sm-',as.integer(width)),
    tags$div(class='box box-solid no-padding am-square',
      tags$div(class='box-body',
        ...
        )
      ))
}


# add dependencies to an existing shiny function
#addUIDep <- function(x) {
#  jqueryUIDep <- htmltools::htmlDependency("jqueryui", "1.10.4", c(href="shared/jqueryui/1.10.4"),
#    script = "jquery-ui.min.js",
#    stylesheet = "jquery-ui.min.css")
#
#  htmltools::attachDependencies(x, c(htmltools::htmlDependencies(x), list(jqueryUIDep)))
#}

# amProgressBar  : display a progressbar
# idBar : div id
amProgressBar<-function(idBar=""){
  div(class="am-progress-container",
    tags$div(id=idBar,class='am-progress-bar',style="width:0%")
    )
}


# function to create accordion in UI
# id= unique html ID
# show = vector of item number to display at start. Ex. show=c(1,2) will not collapse item 1 and 2 at start.
# itemList = list of named list with title and content element. ItemList is a list that contain title, content and optional js condition argument.
# 
amAccordionGroup<-function(id,style=NULL,show=NULL,itemList){
  if(is.null(style)) style <- ""
  cnt=0
  contentList<-lapply(itemList,function(x){
    cnt<<-cnt+1
    ref<-paste0(amSubPunct(id,'_'),cnt)
    showItem<-ifelse(cnt %in% show,'collapse in','collapse')
    stopifnot(!is.list(x) || !is.null(x$title) || !char(x$title)<1 || !is.null(x$content) || !nchar(x$content)<1)
    if(is.null(x$condition))x$condition="true"
    div(style=style,class="panel panel-default",`data-display-if`=x$condition,
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

amCenterTitle = function(title="",h=2,m=50,sub=NULL){ 
  m<-paste0(m,"px")
  s<-paste0("text-align:center;margin-top:",m)
  tagList(
    tags[paste0('h',h)][[1]](style=s,title),
    p(style="text-align:center",sub),
    hr()
    )
}

#' Random name generator
#' 
#' Create a random name with optional prefix and suffix.
#' 
#' @param prefix Prefix. Default = NULL
#' @param suffix Suffix. Default = NULL
#' @param n Number of character to include in the random string
#' @return  Random string of letters, with prefix and suffix
#' @export
randomName <- function(prefix=NULL,suffix=NULL,n=20,sep="_"){
  prefix = amSubPunct(prefix,sep)
  suffix = amSubPunct(suffix,sep)
  rStr = paste(letters[round(runif(n)*24)],collapse="")
  str = c(prefix,rStr,suffix)
  paste(str,collapse=sep)
}



#' Create a modal panel
#'
#' Create a modal panel with some options as custom button, close button, html content. 
#'
#' @param id Panel id
#' @param title Panel title
#' @param subtitle Panel subtitle
#' @param html HTML content of the panel, main text
#' @param listActionButton If FALSE, hide buttons. If NULL, display default close panel button, with text given in defaultButtonText. If list of buttons, list of button.
#' @param defaultButtonText Text of the default button if listActionButton is NULL and not FALSE
#' @param style Additional CSS style for the panel 
#' @param class Additional class for the panel
#' @param hideCloseButton Boolean. Hide the close panel button
#' @param draggable Boolean. Set the panel as draggable
#' @export
amModal<- function(id="default",title=NULL,subtitle=NULL,html=NULL,listActionButton=NULL,addCancelButton=FALSE,background=TRUE,defaultButtonText="Close",style=NULL,class=NULL,hideCloseButton=FALSE,draggable=TRUE,fixed=TRUE){ 

  classModal <- "panel-modal"
  rand <- randomName()

  idBack <- paste(id,rand,"background",sep="_")
  idContent <- paste(id,rand,"content",sep="_")
  jsHide <- paste0("$('#",idContent,"').toggle();$('#",idBack,"').toggle()")
  # If NULL Set default button action to "close" panel, with custom text
  if(is.null(listActionButton))listActionButton=list(
    tags$button(onclick=jsHide,defaultButtonText,class="btn btn-info")
    )
  if(addCancelButton){
  listActionButton <- tagList(
    listActionButton, 
    tags$button(onclick=jsHide,"Cancel",class="btn btn-default")
    )
  }
  # if explicit FALSE is given, remove modal button. 
  if(isTRUE(is.logical(listActionButton) && !isTRUE(listActionButton)))listActionButton=NULL
# close button handling
  if(hideCloseButton){
    closeButton=NULL
  }else{
    closeButton=a(href="#", onclick=jsHide,style="float:right;color:black",icon('times'))
  }

  if(background){
    backg <- div(id=idBack,class=paste("panel-modal-background"))
  }else{
    backg <- character(0)
  }

  if(fixed){
  style = paste("position:fixed",style)
  }else{
  style = paste("position:absolute",style)
  }

  if(draggable){
  scr <- tags$script(sprintf("
    $('#%1$s').draggable({ 
      cancel: '.panel-modal-text'
    });
    ",idContent))
  }else{
  scr = ""
  }

  tagList( 
    backg,
    div( 
      id=idContent,
      class=paste(class,classModal,"panel-modal-content"),
      style=style,
      closeButton,
      div(class=paste('panel-modal-head'),  
        div(class=paste('panel-modal-title'),title)
        ),
      div(class=paste('panel-modal-subtitle'),subtitle),
      hr(),
      div(class=paste('panel-modal-text'),html),
      hr(),
      div(class=paste('panel-modal-buttons'),
        listActionButton
        )
      ),
    scr
    )
}


#' Update existing panel
#'
#' Use output object to update the panel with a known id. E.g. for updating uiOutput("panelTest"), use mxUpdatePanel with panelId "panelTest"
#'
#' @param panelId Id of the existing panel
#' @param session Shiny reactive object of the session
#' @param ... Other amModal options
#' @export
amUpdateModal <- function(panelId=NULL,session=shiny:::getDefaultReactiveDomain(),close=FALSE,...){
  if(!close){
  session$output[[panelId]] <- renderUI(amModal(id=panelId,...))
  }else{ 
  session$output[[panelId]] <- renderUI("")
  }
}

#' R list to html list
#' @param listInput list in inptu
#' @param htL List to append to
#' @param h Value of the first level of html header
#' @param exclude list named item to exclude
#' @export
listToHtmlClass<-function(listInput, exclude=NULL, c=0, htL="",classUl="list-group",classLi="list-group-item"){

#  hS<- '<u>' #start
#  hE<- '</u>' #end
  c = c+1 #next

  if(is.list(listInput)){
    nL <- names(listInput)
    nL <- nL[!nL %in% exclude]
    htL <- append(
      htL,
      paste(
        '<ul class="',
        paste(
          classUl,
          collapse=","
          ),
        '">'
        )
      ) # open
    for(n in nL){
#      htL <- append(htL,c(hS,n,hE))
  htL<-append(
      htL,
      c(
        paste(
          '<li class="',
          paste(classLi,collapse=","),
          '">'
          ),
        n)
      )
      subL <- listInput[[n]]
      htL <- listToHtmlClass(
        subL, 
        exclude=exclude,
        htL=htL,
        c=c,
        classUl=classUl,
        classLi=classLi
        )
    }
    htL<-append(htL,'</li></ul>') # close

  }else if(is.character(listInput) || is.numeric(listInput)){

    htL<-append(
      htL,
      paste("<b>",listInput,"</b>")
     # c(
        #paste(
          #'<b class="',
          #paste(classLi,collapse=","),
          #'">'
          #),
        #paste(
          #listInput,
          #collapse=','
          #),
        #'</li>')
      )

  }
  return(paste(htL,collapse=''))
}
