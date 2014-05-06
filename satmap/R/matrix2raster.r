matrix2raster <- function(z,x,y,layer,proj="+proj=longlat"){
#   #inst.pkg('raster')
  if(class(z) == 'array'){
    cat('\nconverting array to RasterStack object')
    
    if(missing(layer)) layer <- 1:dim(z)[3]
    h <- raster()
    for(ts in layer){
      h.add <- raster(t(z[,,ts])[ncol(z):1,])
      h <- addLayer(h,h.add)
    }
  }else{
    cat('\nconverting matrix to RasterLayer')    
    h <- raster(t(z)[ncol(z):1,])
    
  }
  if(!missing(x) & !missing(y)){
    
    extent(h) <- extent(c(range(x),range(y)))
  }else{
    warning('\nconverting without geographical coordinates!') 
  }
  projection(h) <- proj
  return(h)
}