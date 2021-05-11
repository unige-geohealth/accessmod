#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#    
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

amWriteMarkdown <- function(id,text,session=shiny:::getDefaultReactiveDomain()){
  session$sendCustomMessage('amWriteMarkdown',list(
      id = id,
      text = text
      )
    )
}

amRestart<-function(session = shiny:::getDefaultReactiveDomain()){
  system("touch restart.txt")
  session$sendCustomMessage(
    type="amJsCode",
    list(code='location.reload();')
    )
}

amUiClassList <- function(id, add, remove, session=shiny:::getDefaultReactiveDomain()){
  session$sendCustomMessage(
    type="amUiClassList",
    list(
      id = id,
      add = add,
      remove = remove
      )
    )
}

#https://gist.github.com/xiaodaigh/6810928
# check if use of toggleClass could be a better choice.
amActionButtonToggle <- function(id,session=shiny:::getDefaultReactiveDomain(),disable=TRUE) {
  session$sendCustomMessage(
    type="amBtnDisable",
    list(
      id=id,
      disable=disable
      )
    )
}

amInputToggle <- function(id,session=shiny:::getDefaultReactiveDomain(),disable=TRUE) {
  session$sendCustomMessage(
    type="amInputDisable",
    list(
      id=id,
      disable=disable
      )
    )
}

amActionLinkToggle <- function(id,session=shiny:::getDefaultReactiveDomain(),disable=TRUE) {
  session$sendCustomMessage(
    type="amLinkDisable",
    list(
      id=id,
      disable=disable
      )
    )
}

amSelectizeToggle <- function(id,session=shiny:::getDefaultReactiveDomain(),disable=TRUE) {
  session$sendCustomMessage(
    type="amSelectizeDisable",
    list(
      id=id,
      disable=disable
      )
    )
}

amActionButtonWarningToggle <- function(session=shiny:::getDefaultReactiveDomain(),id,warning=TRUE) {
  addWarning<-paste0("$('#",id,"').addClass('btn-warning').removeClass('btn-default');")
  addDefault<-paste0("$('#",id,"').addClass('btn-default').removeClass('btn-warning');")

  val<-ifelse(warning,addWarning,addDefault)
  session$sendCustomMessage(
    type="amJsCode",
    list(code=val)
    )
}







amFileInputUpdate<-function(id,session=shiny:::getDefaultReactiveDomain(),accepts=NULL,multiple=NULL){
  accepts<-paste(accepts,collapse=',')
  multiple<-ifelse(multiple,'true','false')
  accepts<-paste0("$('input#",id,"').prop('accept','",accepts,"');")
  multiple<-paste0("$('input#",id,"').prop('multiple',",multiple,");")
  val=paste(accepts,multiple)
  session$sendCustomMessage(
    type="amJsCode",
    list(code=val)
    )
}


#
# link selected archive to a new window location. The clientshould as to download.
#TODO: as it's rendered in the same window, it could break shiny application, or reset it. Make sure that's not a problem with standard client. Works with webkit client.
amGetData<-function(session=shiny:::getDefaultReactiveDomain(),dataPath){
  if(!is.null(dataPath) && !dataPath==""){
    #val<-paste0("window.location.assign('",dataPath,"');")
    val<-paste0("downloadFile('",dataPath,"');")
    session$sendCustomMessage(
      type="amJsCode",
      list(code=val)
      )
  }
}

#' Update text by id
#'
#' Search for given id and update content. 
#' 
#' @param session Shiny session
#' @param id Id of the element
#' @param text New text
#' @export
amUpdateText<-function(id,text=NULL,ui=NULL,addId=FALSE,session=shiny:::getDefaultReactiveDomain()){
  if(is.null(text) && is.null(ui)){
    return(NULL)
  }else{
    if(is.null(ui)){
      textb64 <- amEncode(text)
      val=list(
        id = id,
        txt = textb64,
        addId = addId
        )
      session$sendCustomMessage(
        type="amUpdateText",
        val
        )
    }else{
      session$output[[id]] <- renderUI(ui)
    }
  }
}

#' amBusyManage 
#'
#' Manually set or remove busy class to shiny.
#'
#' @param session current shiny session
#' @param busy true/false
#'
#' @export
amBusyManage <- function(session=shiny:::getDefaultReactiveDomain(),busy=FALSE){
  stopifnot(is.logical(busy))
  if(busy){
    js="amAddBusy()"
  }else{
    js="amRemoveBusy()"
  }
  session$sendCustomMessage(type='amJsCode',list(code=js))
}

#' amDebugToJs 
#'
#' @param text text to send to js console.log
#' @return NULL
amDebugToJs<-function(text,session=getDefaultReactiveDomain()){
  js <- jsonlite::toJSON(text)
  session$sendCustomMessage(
    type="amJsDebug",
    list(code=js)
    )
}

#' Save named list of value into cookie
#'
#' Note : don't use this for storing sensitive data, unless you have a trusted network.
#'
#' @param session Shiny session object. By default: default reactive domain.
#' @param cookie Named list holding paired cookie value.
#' @param nDaysExpires Integer of days for the cookie expiration
#' @return NULL
#' @export
amSetCookie <- function(session=getDefaultReactiveDomain(),cookies=list(),nDaysExpires=10L,deleteAll=FALSE){


  stopifnot(is.integer(nDaysExpires))
  stopifnot(is.list(cookies))

  if(is.null(cookies) && !isTRUE(deleteAll)) return()

  res <- list()
  res$deleteAll <- deleteAll
  res$cookies <- cookies
  res$expires <- nDaysExpires

  session$sendCustomMessage(
    type="amSetCookie",
    res
    )
}

#' Update double sortable input
#' @param idInput id of the double sortable input
#' @param list1 left list
#' @param list2 rigth list
#' @param session Shiny session
#' @export
amUpdateDoubleSortableInput <- function(idInput,list1=list(),list2=list(),session= shiny::getDefaultReactiveDomain()){

  listItem1 <- amListToSortableLi(list1)
  listItem2 <- amListToSortableLi(list2)

  id1 <- sprintf("%s_1",idInput)
  id2 <- sprintf("%s_2",idInput)


  amUpdateText(id=id1,listItem1)
  amUpdateText(id=id2,listItem2)

  session$sendCustomMessage("amUpdateSortable",id1)
  session$sendCustomMessage("amUpdateSortable",id2)

}
