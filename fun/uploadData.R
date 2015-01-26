
amUploadTable<-function(dataName,dataFile,dataClass){

  fE<-file_ext(dataFile)
  switch(fE,
    'csv'={
      tbl=read.csv(dataFile, header=TRUE,sep=',')
      if(nrow(tbl)<4 || ncol < 2)
        stop(paste('Error (',basename(dataFile),') : number of rows <4 or number of columns < 2. Make sure that the cells in your table is separated by commas [,] characters (csv=comma separated values) and that your table is more than 4x2 cells.'))
    },
    'xls'=tbl<-read.xls(dataFile),
    'xlsx'=tbl<-read.xls(dataFile) 
    )

  aNames<-acceptColNames[[dataClass]]
  tNames<-tolower(names(tbl))
  names(tbl)<-tNames

  if(!all(aNames %in% tNames))
    stop(paste('Error (',basename(dataFile),') : dataset of class ',dataClass,' should contain columns named ',paste(aNames,collapse=';'),'. The provided file contains those columns :',paste(tNames,collapse=';'),'.'))
  

  dbWriteTable(listen$dbCon,dataName,tbl,overwrite=TRUE)

}








amUploadRaster<-function(dataInput,dataName,dataFiles){
  tryCatch({
    tryReproj=TRUE
    # raster validation.
    validateFileExt(dataFiles,'rast')
    # temp geotiff
    tmpDataPath<-file.path(tempdir(),paste0(dataName,'.tiff'))
    gdalwarp(dataInput,
      dstfile=tmpDataPath,
      t_srs=if(tryReproj){getLocationProj()},
      dstnodata="-9999",
      output_Raster=FALSE,
      overwrite=TRUE,
      verbose=TRUE)
    msg('GDAL finished cleaning.')
    if(file.exists(tmpDataPath)){
      execGRASS('r.in.gdal',
        input=tmpDataPath,
        output=dataName,
        flags=c('overwrite','quiet'),
        title=dataName)
      msg(paste("Manage data:",dataName,'imported in GRASS.'))
    }else{
      stop('Manage data: process aborded, due to unresolved CRS or not recognized input files. Check files meta   data and extent. Importation cancelled.')
    }
    # create a rasterlayer to get projection info from the file
    # (raster not loaded in memory)
    r<-raster(tmpDataPath)
    givenProj<-proj4string(r)
    if(!givenProj==getLocationProj()){
      msg(paste(
          "Manage data warning : CRS of ",
          dataName,
          "did not match exactly the CRS of current project."))
      msg(paste(
          "Manage data info. ",
          dataName,
          "Raster's proj4string:",
          givenProj,
          ". Accessmod current proj4string:",
          getLocationProj()))
    }
    # get random input to reactivate  object dependent on this function
  },error=function(cond){
    unlink(dataFiles)
    hintBadProjection<-'Projection of dataset does not appear to match current location.'
    cndMsg <- conditionMessage(cond)
    if(length(grep(hintBadProjection,cndMsg))>0){
      msg('ERROR: The data projection is wrong or absent. Please match it with the base data (DEM)')
    }else{
      msg(cond)
    }
  })
  unlink(dataFiles)
  return(NULL)
}



amUploadVector<-function(dataInput, dataName, dataFiles){
  tryReproj=TRUE
  #fE<-file_ext(dataNew$name)
  # helper function to validate file based on extension
  validateFileExt(dataFiles,'vect')
  tmpDataPath<-file.path(tempdir(),paste0(dataName,'.shp'))
  ogr2ogr(
    src_datasource_name=dataInput,
    dst_datasource_name=tmpDataPath,
    #where=input$dataSql,
    f="ESRI Shapefile",
    t_srs=if(tryReproj){getLocationProj()},
    overwrite=TRUE,
    verbose=TRUE)
  msg('GDAL finished cleaning. Importation in GRASS.')
  tryCatch({
    execGRASS("v.in.ogr",
      flags=c("overwrite","w","r","2"), # overwrite, lowercase, current region, 2d only,
      parameters=list(dsn=tmpDataPath, output=dataName, snap=0.0001)
      )
    msg(paste('Module import:',dataName,'Imported in GRASS.'))
  },
  error=function(cond){
    unlink(dataFiles)
    hintBadProjection<-'Projection of dataset does not appear to match current location.'
    cndMsg <- conditionMessage(cond)
    badProjection<-if(length(grep(hintBadProjection,cndMsg))>0){
      msg('Error: the data projection is wrong or absent. Please match it with the base data (DEM)')
    }else{
      msg(cond)
    }
  })
  unlink(dataFiles)
  return(NULL)
}

#
#  # validate rules of input file.
#  #fE<-file_ext(dataNew$name)
#  # helper function to validate file based on extension
#  validateFileExt(dataInput,'vect')
#  tmpDataPath<-file.path(tempdir(),paste0(dataName,'.shp'))
#  ogr2ogr(
#    src_datasource_name=dataInput,
#    dst_datasource_name=tmpDataPath,
#    where=input$dataSql,                                                                                                  f="ESRI Shapefile",
#    t_srs=if(tryReproj){getLocationProj()},
#    overwrite=TRUE,
#    verbose=TRUE)
#  msg('GDAL finished cleaning. Importation in GRASS.')
#  tryCatch({
#    execGRASS("v.in.ogr",
#      flags=c("overwrite","w","r","2"), # overwrite, lowercase, current region, 2d only,
#      parameters=list(dsn=tmpDataPath, output=dataName, snap=0.0001)
#      )
#    unlink(lF)
#    msg(paste('Module import:',dataName,'Imported in GRASS.'))
#    listen$uploadData<-sample(100,1)
#  },
#  error=function(cond){
#    file.remove(lF)
#    hintBadProjection<-'Projection of dataset does not appear to match current location.'
#    cndMsg <- conditionMessage(cond)
#    badProjection<-if(length(grep(hintBadProjection,cndMsg))>0){
#      msg('ERROR: The data projection is wrong or absent. Please match it with the base data (DEM)')
#    }else{
#      msg(cond)
#    }
#  }
#  )
