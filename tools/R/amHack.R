#
# HACK time. Replace base method that fails..
#
be <- as.environment('package:base') # same as baseenv()

#' Reset TZ variable before format 
#' ⚠️ Something write TZ env variable and corrupt it,
#' probably https://github.com/wch/r-source/blob/tags/R-4-1-1/src/main/datetime.c
#' We can't do anything excepts rewrite back the correct TZ...
if(!existsFunction('am_format')){

  #
  # Reset TZ
  #
  reset_tz <- function(){
    tz <- options('tz')
    if(is.null(tz)){
      tz = 'UTC'
    }
    if(!identical(Sys.getenv('TZ'),tz)){
      Sys.setenv(TZ=tz);  
    }
  }

  #
  # Replace base format
  #
  am_format <- base::format 
  unlockBinding('format',be)
  be$format <- function(...){
    reset_tz()
    am_format(...)
  }
  lockBinding('format',be)
}


#
# gsub will fail if encoding is not utf8: it happens
# with the TZ issue. Uncomment this to intercept here
#
if(FALSE && !existsFunction('am_gsub')){
  #
  # Replace base gsub
  #
  am_gsub <- base::gsub
  unlockBinding('gsub',be)
  be$gsub <- function(...){
    ee <- environment()
    tryCatch({
      am_gsub(...)
    },
    error=function(e){
      #
      # Use parent.frame(1-4) and .tracestac(1-5) to
      # to inspect what's wrong
      #
      #browser()
      stop(e)
  })}
  lockBinding('gsub',be)
}



