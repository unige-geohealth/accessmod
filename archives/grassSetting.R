


grassAutoConfig<-function(sg, ){
  require(spgrass6)


# Check if we need new grass region.
# if TRUE the first raster will be the default WIND.
if(get.GIS_LOCK()=='' || !exists('gisLock')){
  # read only meta data.
  sg<-raster(ras$tmp,layer=0)
  # empty grid for the default WIND object
  sg<-as(r,'SpatialGrid')
  # init grass 
  initGRASS(gisBase = grassBase, # binary files
            gisDbase = grassDataBase, # local grass database
            location = grassLocation, # rsession
            mapset= grassMapset, # PERMANENT
            SG=sg,
            override=TRUE)
  execGRASS('g.proj',flags='c',proj4=proj)
  assign('gisLock',get.GIS_LOCK(),envir=globalenv())
  gisLock=get.GIS_LOCK()
  # set as default region
  execGRASS('g.gisenv',flags='s')
}



initGRASS(gisBase = base,
  )

}
