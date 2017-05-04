#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/



#' encode in base64
encodeB64 <- function(text){

if(length(text)!=1) text="[NA]"
  base64enc::base64encode(charToRaw(as.character(text)))
}

progressBarControl <- function(id=config$pBarId,percent=0,title="default",text="default",tooltip="",visible=TRUE,session=getDefaultReactiveDomain(),timeOut=NULL){

  httpuv:::service()
  quit = isTRUE(session$input$cleanExit)

  # default time out
  if(amNoDataCheck(timeOut) || !is.numeric(timeOut)){
    timeOut <- config$pBarTimeOut
  }

  if(quit){
    session$sendCustomMessage(type="progressUpdate",list(
        visible=visible,
        id=id,
        percent=100,
        title=encodeB64(title),
        text=encodeB64("Interruption")
        )) 
    stop("pBarQuit")
  }else{
    session$sendCustomMessage(type="progressUpdate",list(
        visible=visible,
        id=id,
        percent=percent,
        title=encodeB64(title),
        text=encodeB64(text)
        ))
  }

}
pbc <- progressBarControl 



#progressBarUi <- function(id="test",zIndex=1201,listActionButton=NULL,defaultButtonText="close",addCancelButton=FALSE,classButtons="",showOptions=T){
  #idPanel <- sprintf("progress-panel-for-%s",id)
  #idBar <- sprintf("progress-for-%s",id)
  #idBarContainer <- sprintf("progress-container-for-%s",id)
  #idText <- sprintf("progress-text-for-%s",id)
  #idTitle <- sprintf("progress-title-for-%s",id)
  #idTooltip <- sprintf("progress-tooltipbar-for-%s",id)
  #classHidden <- "pbar-hidden"
  #jsHide <- sprintf("$('#%1$s').addClass('%2$s')",idPanel,classHidden)



  ## If NULL Set default button action to "close" panel, with custom text
  #if(is.null(listActionButton) && !is.null(defaultButtonText) && !isTRUE(addCancelButton)){
    #listActionButton=list(
    #tags$button(onclick=jsHide,defaultButtonText,class=paste("btn btn-default",classButtons))
    #)
  #}else{
    #listActionButton = tagList()
  #}
  
  #if(addCancelButton){
  #listActionButton <- tagList(
    #listActionButton, 
    #tags$button(onclick=jsHide,"Cancel",class=paste("btn btn-default",classButtons))
    #)
  #}
  ## if explicit FALSE is given, remove modal button. 
  #if(isTRUE(is.logical(listActionButton) && !isTRUE(listActionButton)))listActionButton=NULL

  #if(showOptions){


    
    #listOptions <- tags$span(class="pbar-options-container",
      #tags$span(class="pbar-options-content",icon("gear"),
      #tags$ul(class="nav pbar-options",
        #tags$li(tags$button(class="btn pbar-option","STOP PROCESS",onclick="
            #Shiny.onInputChange('cleanExit',true); 
            #$('#pBarExit').removeClass('pbar-hidden');
            #")
            #)
        #)
      #)
      #) 

  #}else{
    #listOptions <- tagList()
  #}

  ## return html
  #tagList( 
    #singleton(
      #tags$head(
        #tags$script(src="progress/progress.js",type="text/javascript"),
        #tags$link(href="progress/progress.css",rel="stylesheet",type="text/css")
        #)
      #),
    #div(id=idPanel,style=sprintf("z-index:%s",zIndex),class="pbar-panel pbar-hidden",
      #div(class="col-xs-4"),
      #div(class="col-xs-4 pbar-container",
        #h3(id="pBarExit",class="pbar-hidden","Stop process as soon as possible, please wait..."),
        #div(class="pbar-content", 
          #div(id=idBarContainer,class="pbar-bar-container",
            #div(id=idBar,class="pbar-bar")
            #),
          #div(
            #img(src="progress/spin.gif",style="width:16px;height:16px;margin-top:-5px"),
            #span(class="pbar-title",id=idTitle,"title"),
            #span(class="pbar-text",id=idText,"text")
            #),

          #listActionButton
          #)
        #),
      #div(class="col-xs-4",
        #listOptions
        #)
      #)
    #)
#}


