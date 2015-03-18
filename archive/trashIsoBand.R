

if(FALSE){
  tmpTiff<-tempfile(fileext='.tiff')
  tmpShp<-file.path(tempdir(),'contour.shp')
  tmpJs<-file.path(tempdir(),'contour.js')
  tmpJson<-file.path(tempdir(),'contour.json')
  tmpJsonLatLong<-file.path(tempdir(),'contourLatLong.json')
  
  outJson<-file.path('~/Downloads/isobands.json')
  execGRASS('r.mask',raster=tmpCost)
  execGRASS('r.mapcalc',expression=paste(tmpCost,'=',tmpCost),flags='overwrite')
  execGRASS('g.region',raster=tmpCost,zoom=tmpCost)
 execGRASS('r.out.gdal',
            flags =c('overwrite','f'),
            input=tmpCost,
            output=tmpTiff,
           format='GTiff' 
            )
 execGRASS('r.to.vect',flags='overwrite',input=tmpCost,output='tmp__cost_vect',type='point',column='cost')
 execGRASS('v.out.ogr',input='tmp__cost_vect',output=tmpJson,format='GeoJSON')

 ogr2ogr(tmpJson,tmpJsonLatLong,t_srs=listen$mapMeta$latlong$proj,f='GML',overwrite=T,zfield='cost')
 req<-sprintf("
 var turf = require('turf-isobands');
 var fs = require('fs');
 var points = fs.readFileSync('%s');
 var breaks = [0,3000,5000,7200];
 var isolined = turf.isobands(points, 'z', 10, breaks);
 fs.writeFileSync('%s', JSON.stringify(isolined)); 
 ",tmpJsonLatLong,outJson)

 write(req,tmpJs)

system(sprintf('node -e "%s"',req))


r.to.vect input=tmp_c@p_500_m output=tmp_c_vect type=point column=cost
 system(paste('python python/isobands_matplotlib.py -i 600',tmpTiff,tmpShp))




 r<-raster(tmpFile)
 contourf <- function(
   x = NULL,
   zlim = cellStats(x,'range'),
   col = par("fg"),
   fill.col,
   fill.alpha = "80",   # hex value for alpha transparency
   #    lty = par("lty"), lwd = par("lwd"),
   add = FALSE, ...) {

   contour(x, levels=levels, zlim=zlim, col=col, 
     add=add, ...)
   line.list <- rasterToContour(r, nlevels=nlevels, levels=levels)
   # contourLines returns a list of lists, each with components
   # 'level', 'x', 'y'

   if(missing(fill.col)) {
     colramp = colorRampPalette(c("white", col))
     fill.col <- paste(colramp(5), fill.alpha, sep="")
   }
   for (i in seq_along(line.list)) {
     polygon(line.list[[i]][2:3], col=fill.col[i], border=NA)
   }
   invisible(line.list)
 }


levelQuant = quantile(r)
zlim=cellStats(r,'range')
test<-contour(r,levels=levelQuant,zlim=zlim)
line.list<-rasterToContour(r,nlevels=length(levels),levels=levelQuant)

for (i in seq_along(line.list)) {
     polygon(line.list[[i]][2:3], border=NA)
   }





xy<-levelplot(r)$panel.args.common[c(x='x',y='y')]
 test<-SpatialPolygons(list(Polygons(list(Polygon(matrix(c(xy$x,xy$y),ncol=2))),ID='test')))
 system('python fun/isobands_matplotlib.py ')



}

# contour zones by quantiles
if(FALSE){
  tmpTbl<-tempfile()
  system.time({
    qZ<-quantile(tblPopByZone$zone,seq(0,1,0.1))
    iZ<-findInterval(tblPopByZone$zone,qZ,rightmost.closed=T)
    tblPopByZone$qZone<-iZ
    tZ<-aggregate(sum~qZone,tblPopByZone,FUN=sum)
    tZ$zMax<-qZ[tZ$qZone+1]
    tZ$zMin<-qZ[tZ$qZone]
    tZ$cumSum<-cumsum(tZ$sum)
    write(paste(tZ$zMin,"thru",tZ$zMax,'=',tZ$zMax,collapse='\n'),tmpTbl)
    execGRASS('r.reclass',input=tmpCost,output='tmp__cost_quant',rules=tmpTbl,flags='overwrite')





    n=0
    for(q in qZ[-1]){
      # extract value by quantile of zone
      execGRASS('r.mapcalc',expression=
        paste('tmp__cost_quant_u','=if(tmp__cost_quant<=',round(q),',',round(q),',null())')
        ,flags='overwrite')
      # create vector version of step catchment
      execGRASS('r.to.vect',
        input='tmp__cost_quant_u',
        output='tmp__vect_catch_u',
        type='area',
        column='amTmax',
        flags=c('overwrite'))
      # produce a genarized version
      execGRASS('v.generalize',
        flags='overwrite',
        input='tmp__vect_catch_u',
        output=paste0('tmp__vect_catch_u_smooth_',n),
        method='snake',
        threshold=listen$mapMeta$grid$`North`*2,
        alpha=0.4,
        beta=0.4
        )
      if(n>0){
        execGRASS('v.category', input=paste0('tmp__vect_catch_u_smooth_',n),output='tmp__vect_catch_u_smooth_0', layer=paste(n),option='add',flags='overwrite')
      }
      n=n+1
    }

    tmpMapsQuant<-execGRASS('g.list',type='vect',pattern='tmp__vect_catch_u_smooth_*',intern=T)
    execGRASS('v.patch',flags=c('overwrite','e'),input=tmpMapsQuant,output='tmp__vect_catch_patched')
    tmpMap<-tempfile()
    execGRASS('v.out.ogr',)

#v.out.ogr input=tmp__vect_catch_patched@p_500_m output=test format=ESRI_Shapefile

 #   tmpPatchedMap<-readVECT6('tmp__vect_catch_patched')

   




  #v.generalize --overwrite input=tmp__vect_catch_u@p_500_m output=tmp__vect_catch_u_smooth method=chaiken threshold=2000

   # execGRASS('r.to.vect',input='tmp__cost_z',output='tmp__vect_catch',type='area',column='amTmax',flags=c('s','overwrite'))
  })



}
