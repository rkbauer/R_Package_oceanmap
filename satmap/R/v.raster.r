v.raster <- function(obj, varname, layer=1, param=varname, zlim, minv, maxv, adaptive.vals, replace.na=F, Log, main, 
                     cbx, cby, cb.title, cb.xlab, pal, steps,
                     sidelabels=F, Ylab=F, axeslabels=T, ticklabels=T, cex.lab=0.8, cex.ticks=0.8, 
                     subplot=F, width, height, figdim, xpos=-1, Save=F, plotfolder=".", plotname, fileformat="png", suffix, 
                     region, v_area=region, v_image=T, v_contour=F, levels, contour.labels=NULL, v_arrows=F, scale_arrow=1, 
                     fill=T, col="grey", border='black', grid=T, grid.res, bwd=1,
                     dates){
  if(missing(param)) param <- ''
  if(missing(varname)) varname <- param
  if(!missing(width) & !missing(height)) figdim <- c(width, height)
  
  region_definitions <- NULL
  rm(region_definitions)
  data('region_definitions',envir=environment())
  
  parameter_definitions <- NULL
  rm(parameter_definitions)
  data('parameter_definitions',envir=environment())
  
  param_def <- parameter_definitions[as.character(parameter_definitions$param) == param,]
  if(nrow(param_def) == 0) param_def <- as.data.frame(matrix(NA,nrow=1,ncol=ncol(param_def),dimnames=list(1,names(param_def))))
  if(missing(Log)) Log <- F
  param_def$log <- as.numeric(Log)
  if(missing(cb.xlab) & is.na(param_def$name1)) cb.xlab <- varname
  param <- param_def$param <- varname
  
  #   #inst.pkg(raster)
  if(!(grepl('Raster', class(obj)))) stop('error in.v.raster.r: obj of unknown input format. Please check!')   
  obj <- brick(obj) # convert RasterLayer to RasterBrick object to conduct same operation
  if(missing(adaptive.vals)) adaptive.vals <- T  
  
  if(missing(v_area)){
    ext <- extent(obj)
    v_area.rounded <- round(as.vector(t(bbox(ext))),digits=1)
    
    ## try to reconstruct v_area (looking for regions with the same extent)
    ext2 <- as.vector(t(bbox(ext)))[c(4,3,1:2)]
    id <- which(apply(region_definitions[,3:6],1,function(x) all(ext2 %in% x)))
    if(length(id) > 0){
      v_area <- as.character(region_definitions$label[id[1]])
      v_area.valid <- T # v-area reconstructed
    }else{
      v_area.valid <- F
      cat('\nno region (v_area) defined, run add.region to add region definitions and to save settings for the plot region, colorbar and window size!\n')
      
      v_area.name <- paste0('lon',v_area.rounded[1],'-',v_area.rounded[2],'.lat',v_area.rounded[3],'-',v_area.rounded[4]) # set area name to be plotted
      # #       if(missing(grid.res)) grid.res <- 1 # set grid resolution
      r <- as.data.frame(matrix(NA,ncol=8,nrow=2))
      names(r) <- c("xlim","ylim","dim","name","cbx","cby","figdim","grid.res")
      # customize colorbar
      cb <- cust.colorbar(ext,cbx=cbx,cby=cby,figdim=figdim,input.mode="i",force.figdim.widget=F,xpos=xpos)
      
      r$cbx <- cb$cbx
      r$cby <- cb$cby
      r$align <- cb$align
      r$figdim <- cb$figdim
    }
  }else{
    v_area.valid <- T
  }
  if(v_area.valid){
    r <- regions(v_area)
    area.extent <- extent(c(range(r$xlim),range(r$ylim)))
    if(dim(obj)[3] > 1){
      obj <- crop(obj,area.extent)
    }else{
      obj <- brick(crop(raster(obj,layer=1),area.extent))
    }           
    v_area.name <- v_area # set area name to be plotted      
    if(missing(grid.res)) grid.res <- r$grid.res[1] # set grid resolution
  }
  
  if(missing(dates)) dates <- apply(as.matrix(names(obj)),1,function(x)tail(strsplit(x,"X")[[1]],1))
  if(any(dates == 'layer')) dates <- rep(NA,length(dates))
  
  if(length(dates) <= 1){
    timestep = "1d"
  }else{
    k <- as.POSIXct(dates,format="%Y%m%d%H")
    k.diff <- diff(k)
    k.val <- round(k.diff)[[1]]
    k.unit <- substr(attributes(k.diff)$units,0,1)
    
    if(k.val %in% c(28:31) & k.unit == "d"){
      k.val <- 1
      k.unit <- "m"
    }
    timestep <- paste0(k.val,k.unit)
  }
  #   if(missing(layer)) layer <- dim(obj)[3]
  
  for(ts in layer){
    file_def <- list(source='raster',timestep=timestep,date1=dates[which(layer %in%ts)],date2=dates[which(layer %in%ts)])
    outfile.name <- paste0(v_area.name,"_",param)
    if(!is.na(file_def$date1)) outfile.name <- paste0(outfile.name,"_",file_def$date1)
    b <- raster(obj,layer=ts)
    
   .v.plot(b=b, minv=minv, maxv=maxv, zlim=zlim, adaptive.vals=adaptive.vals, v_area=v_area, replace.na=replace.na, main=main, cb.title=cb.title, cb.xlab=cb.xlab, pal=pal, steps=steps,
           sidelabels=sidelabels, Ylab=Ylab, 
           axeslabels=axeslabels, ticklabels=ticklabels, cex.lab=cex.lab, cex.ticks=cex.ticks, 
           subplot=subplot, xpos=xpos, Save=Save, plotfolder=plotfolder, plotname=plotname, fileformat=fileformat,
           param=param, param_def=param_def, file_def=file_def, r=r, outfile.name=outfile.name,
           # v_area, 
           suffix,
           v_image=v_image, v_contour=v_contour, levels=levels, contour.labels=contour.labels, v_arrows=F,
           fill=fill, col=col, border=border, grid=grid, grid.res=grid.res, bwd=bwd)
  }
}
