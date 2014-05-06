raster2matrix <- function(RasterLayer){
  if(dim(RasterLayer)[3] <= 1){
    m <- t(as.matrix(RasterLayer)[dim(RasterLayer)[1]:1,])
  }else{
    m <- aperm(as.array(flip(RasterLayer,direction=2)),c(2,1,3))
  }
  return(m)
}

raster2array <- function(RasterLayer){
  m <- raster2matrix(RasterLayer)
  return(m)
}