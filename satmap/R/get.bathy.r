v.bathy <- get.bathy <- function(v_area,lon,lat,resolution=4, keep=F, savename.bathy, folder.bathy,visualize=T,...#steps,levels,
                    ){
  if(missing(lon) | missing(lat)){
    
    if(!missing(v_area)){
      if(grepl('Raster', class(v_area)) | grepl('Extent', class(v_area))){
        ext <- as.vector(t(bbox(extent(v_area))))
        lon <- ext[1:2]
        lat <- ext[3:4]
        v_area <- bathy.area <- paste0('lon',lon[1],'-',lon[2],'.lat',lat[1],'-',lat[2])
      }else{
        r <- regions(v_area)
        lon <- r$xlim
        lat <- r$ylim
        bathy.area <- v_area
      }
    }
  }
  if(missing(lon) | missing(lat))stop('geographical reference missing! please revise!')
  if(missing(v_area)) {
    bathy.area <- paste0('lon',lon[1],'-',lon[2],'.lat',lat[1],'-',lat[2])
  }else{
    bathy.area <- v_area
  }
  if(missing(folder.bathy)) folder.bathy <- getwd()
  
  if(missing(savename.bathy)){
    savename.bathy <- paste0('bathy_',bathy.area,'_res.',resolution,'.dat')
  }else{
    if(!grepl(".dat", savename.bathy)) savename.bathy <- paste0(savename.bathy,'.dat')
  }
  folder.bathy <- .check.folder(folder.bathy)
  savename <- paste0(folder.bathy,savename.bathy)
  
  if(file.exists(savename)){
    load(savename)
  }else{
    bathy <- getNOAA.bathy(lon1 = min(lon), lon2 = max(lon), lat1 = min(lat), lat2 = max(lat),
                           resolution = resolution)   
    h <- bathy
    h <- t(h[])[ncol(h):1,]
    h <- raster(h)
    extent(h) <- extent(c(lon,lat))
    projection(h) <- "+proj=longlat"
    h[h > 0] <- NA
    h <- -1*h
  }
  if(keep) {
    save(h, file=savename)
    cat(paste0("\nsaving bathymetry as: '",savename.bathy, "' in folder: '", folder.bathy, "'\n")) # display files to print
  }
  
  if(visualize) v(h,param='bathy',...)
  return(h)
}
