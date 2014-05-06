
# last update: 24.sept.2013

setClass('bathy')
setClass('gz')
setClass('nc')
setClass('ncdf4')

# if ( !isGeneric("v") ) {
#   setGeneric("v", function(x, ...)
#     standardGeneric("v"))
# }

v <- function(obj, ...) UseMethod("v")

setMethod('v', signature(obj='character'), 
          function(obj,...){
            if(length(obj) == 0) stop(paste('No file corresponds to search string entered! Please revise or .check.folder!'))
            obj.opt <- 1
            for(s in obj){
              if(s == 'bathy'){
                class(obj.opt) <- 'bathy'
                v(obj=obj.opt,...)
              }else{
                files <- Sys.glob(s)
                if(length(files) == 0) stop(paste('No file corresponds to search string entered! Please revise or .check.folder!'))
                for(files.sub in files){
                  if(grepl('.gz', files.sub, fixed=T)){ # fixed necessary since otherwise "point" is not recognized.
                    class(files.sub) <- 'gz'
                    v(obj=files.sub,...)
                  }
                  if(grepl('.nc', files.sub, fixed=T)){ # fixed necessary since otherwise "point" is not recognized.
                    class(obj) <- 'nc'
                    v(obj,...)
                    
                  }
                }
              }
            }
          }
)

# setMethod('v', signature(obj='.bathy'), function(obj, ...) v.bathy(visualize=T, ...))
setMethod('v', signature(obj='bathy'), 
          function(obj, v_area, lon, lat, resolution=4, keep=F, 
                   savename.bathy, folder.bathy=".", adaptive.vals=T, cb.title, ...) v.bathy(v_area=v_area,lon=lon,lat=lat,resolution=resolution, keep=keep,
          savename.bathy=savename.bathy, folder.bathy=folder.bathy,visualize=T,cb.title=cb.title,...))

setMethod('v', signature(obj='gz'), function(obj, v_area, adaptive.vals=F, ...) v.gz(obj=unique(obj), v_area=v_area, adaptive.vals=adaptive.vals, ...))
  

setMethod('v', signature(obj='nc'), 
          function(obj, varname, t=1, adaptive.vals=T, dates, 
                   cb.xlab=varname, lonname="lon", latname='lat',...){
            obj2 <- nc2raster(obj,varname,lonname=lonname,latname=latname,layer=t,date=T)
            
          v.raster(obj=obj2, layer=1:length(t),adaptive.vals=adaptive.vals,...)
          }
)

setMethod('v', signature(obj='ncdf4'), 
          function(obj, varname, t=1, adaptive.vals=T, dates, 
                   cb.xlab=varname, lonname="lon", latname='lat',...){
            obj2 <- nc2raster(obj,varname,lonname=lonname,latname=latname,layer=t,date=T)
          v.raster(obj=obj2, layer=1:length(t),adaptive.vals=adaptive.vals,...)
          }
)

setMethod('v', signature(obj='RasterStack'), function(obj, varname, t=1, ...)v.raster(obj=obj, varname=varname,layer=t,...))
setMethod('v', signature(obj='RasterBrick'), function(obj, varname, t=1, ...)v.raster(obj=obj, varname=varname,layer=t,...))
setMethod('v', signature(obj='RasterLayer'), function(obj, varname, t=1, ...)v.raster(obj=obj, varname=varname,layer=t,...))








