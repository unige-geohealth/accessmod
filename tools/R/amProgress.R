
#' encode in base64
encodeB64 <- function(text){
stopifnot(require(base64enc))
  base64enc::base64encode(charToRaw(as.character(text)))
}


progressBarUi <- function(id="test",zIndex=1201,listActionButton=NULL,defaultButtonText="close",addCancelButton=FALSE,classButtons=""){
  idPanel <- sprintf("progress-panel-for-%s",id)
  idBar <- sprintf("progress-for-%s",id)
  idBarContainer <- sprintf("progress-container-for-%s",id)
  idText <- sprintf("progress-text-for-%s",id)
  idTitle <- sprintf("progress-title-for-%s",id)
  idTooltip <- sprintf("progress-tooltipbar-for-%s",id)
  classHidden <- "pbar-hidden"
  jsHide <- sprintf("$('#%1$s').addClass('%2$s')",idPanel,classHidden)



  # If NULL Set default button action to "close" panel, with custom text
  if(is.null(listActionButton) && !is.null(defaultButtonText) && !isTRUE(addCancelButton)){
    listActionButton=list(
    tags$button(onclick=jsHide,defaultButtonText,class=paste("btn btn-default",classButtons))
    )
  }else{
    listActionButton = tagList()
  }
  
  if(addCancelButton){
  listActionButton <- tagList(
    listActionButton, 
    tags$button(onclick=jsHide,"Cancel",class=paste("btn btn-default",classButtons))
    )
  }
  # if explicit FALSE is given, remove modal button. 
  if(isTRUE(is.logical(listActionButton) && !isTRUE(listActionButton)))listActionButton=NULL


  # return html
  tagList( 
    div(id=idPanel,style=sprintf("z-index:%s",zIndex),class="pbar-panel pbar-hidden",
      div(class="col-xs-4"),
      div(class="col-xs-4 pbar-container",
        div(class="pbar-content", 
                  div(id=idBarContainer,class="pbar-bar-container",
            div(id=idBar,class="pbar-bar")
            ),
            div(
            img(src="progress/spin.gif",style="width:16px;height:16px;margin-top:-5px"),
            span(class="pbar-title",id=idTitle,"title"),
            span(class="pbar-text",id=idText,"text")
            ),

        listActionButton
          )
      ),
    div(class="col-xs-4")
    ),
  singleton(
    tags$head(
      tags$script(src="progress/progress.js",type="text/javascript"),
      tags$link(href="progress/progress.css",rel="stylesheet",type="text/css")
      )
    )
  )
}


progressBarControl <- function(id=config$pBarId,percent=0,title="default",text="default",tooltip="",visible=TRUE,session=getDefaultReactiveDomain(),timeOut=NULL){
  # default time out
  if(amNoDataCheck(timeOut) || !is.numeric(timeOut)){
  timeOut <- config$pBarTimeOut
  }
    dat <- list(
      visible=visible,
      id=id,
      percent=percent,
      title=encodeB64(title),
      text=encodeB64(text),
      tooltip=encodeB64(tooltip)
      )
    session$sendCustomMessage(type="progressUpdate",dat)
    Sys.sleep(timeOut)
}
pbc <- progressBarControl 



