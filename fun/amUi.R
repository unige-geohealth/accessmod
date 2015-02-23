


library(shiny)
library(shinydashboard)
library(leaflet)
library(htmltools)

source('fun/handsontable.R')


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
