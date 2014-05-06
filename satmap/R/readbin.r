##inst.pkg("reshape")
##inst.pkg("fields")

readbin <- function(filename,area,Image=F,byte=F,Raster=T){
  #inst.pkg("raster")
  #inst.pkg("fields")
  #open connection to the gz file
  filename <- Sys.glob(filename)
  if(length(filename) == 0) stop('search string does not match. please check!')
  if(length(filename) == 1){
    m <- .readbin.single(filename=filename, Image=Image,byte=byte,Raster=Raster,area=area)
  }else{
    warning('loading multiple files!')
    
    if(nrow(unique(name_split(filename)[,c(1,3:4)])) > 1){
      print(check_gzfiles(filename))
      stop('non unique region, parameter or resolution selected. please check!')
    }
    if(Raster){
      cat('combining files as Raster-Layers!')
      for(f in 1:length(filename)){
        fname <- filename[f]
        layer <- .readbin.single(filename=fname, Image=Image,byte=byte,Raster=T,area=area)
        if(f == 1){
          m <- layer
        }else{
          m <- stack(m, layer)
        }
      }
      names(m) <- name_split(filename)$date1
    }else{
      cat('combining files as multi-dimensional array!')
      
      for(f in 1:length(filename)){
        fname <- filename[f]
        layer <- .readbin.single(filename=fname, Image=Image,byte=byte,Raster=F,area=area)
        if(f == 1){
          m <- array(layer,c(dim(layer),1))
        }else{
          m <- abind(m, layer,along=3)
        }
      }
    }
  }
  return(m)
}

.readbin.single <- function(filename, Image=F,byte=F,Raster=T,area){
  filename_splitted <- unlist(strsplit(filename,"\\_"))
  
  param <- filename_splitted[3]
  SIZE <- switch((param == "bathy")+1,1,2)
  
#   filename <- system(paste("ls ", file,sep=""),intern=T)
  confile <- gzfile(filename,"rb")
  # n values can be given precisely :
  # a<-readBin(confile,integer(),n=3360*1440,size=1,signed=F)
  # or not (but memory dependent)
  a <- readBin(confile,integer(),n=10e6,size=SIZE,signed=F) # before 10e8
  #signed T or F : numerical values between -128 to 128 or 1 to 256
  #close the connection to the file
  close(confile)
  
#   filename_splitted <- name_split(filename)
  region <- filename_splitted[1]#unlist(lapply(filename_splitted, function(x) substr(x[1],1,100))))
  r <- regions(region)
  ncol=r$dim[1]
  nrow=r$dim[2]
  if(param == "bathy")
  {
#     savename <- '/home/robert/Dropbox/Ifremer/scripts/R/sat_package/regions.dim.bathy.RData'
#     load(savename)
#     data('regions.dim.bathy')
    
    data(sysdata, envir=environment())
    regions.dim.bathy.sub <- regions.dim.bathy[[filename_splitted[1]]]
    nrow <- regions.dim.bathy.sub$dim[1]
    ncol <- regions.dim.bathy.sub$dim[2]
    r[,1:2] <- t(bbox(regions.dim.bathy.sub$extent))
    #matrix conversion with reverse rows (R matrix spec)
    b <- matrix(a,ncol,nrow)[,nrow:1]  
  }else{
    b <- matrix(a,ncol,nrow)[,nrow:1]  
  # remove undesired regions 
#   switch(region,
#          "med4"={b[1:155,307:384]=NA; b[700:ncol,267:nrow]=NA}, # remove bay of bsicay, black sea
#          "medw4"={b[1:155,207:nrow]=NA; b[430:ncol,200:nrow]=NA; b[480:ncol,170:nrow]=NA} # remove bay of bsicay, adrian sea
#   )
  }
  #b[1:155,307:384]=NA
  if(byte)
  {
     b[which(b>252)]=NA
#      default encoding sets fronts to 252 (before front_gauss); landmask to 253, clouds to 255
  }else{
    param <- filename_splitted[3]#filename_splitted$parameter#unique(unlist(lapply(filename_splitted, function(x) substr(x[3],1,100))))
    b <- param_convert(b,param)
  }
  
  #rownames(b)=paste(1:nrow(b))
  #reshapes matrix into columns
  #b2=melt(b)
  h <- b
  
  h <- matrix2raster(b,x=r$xlim,y=r$ylim)
#   h <- raster(t(b)[ncol(b):1,],xmn=r$xlim[1],xmx=r$xlim[2],ymn=r$ylim[1],ymx=r$ylim[2])

  if(!missing(area)){
    r <- regions(area)
    area.extent <- extent(c(range(r$xlim),range(r$ylim)))
    h <- crop(h,area.extent)
  }
  if(!Raster) h <- raster2matrix(h)
#   r <- regions(area)
#   h.extent
  
  if (Image==T){
    image.plot(h, xlab="longitude", ylab="latitude",xlim=r$xlim,ylim=r$ylim, col=heat.colors(100))
  }
  return(h)
}
