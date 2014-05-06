v.gz <- function(obj, v_area=region, adaptive.vals=F, param, zlim, minv, maxv, replace.na=F, Log, main, cbx, cby, cb.title, cb.xlab, pal, steps,
                 sidelabels=F, Ylab=F, axeslabels=T, ticklabels=T, cex.lab=0.8, cex.ticks=0.8, 
                 subplot=F, width, height, figdim, xpos=-1, Save=F, plotfolder=".", plotname, fileformat="png", suffix='', 
                 region, v_image=T, v_contour=F, levels, contour.labels=NULL, v_arrows=F, scale_arrow=1, 
                 fill=T, col="grey", border='black', grid=T, grid.res, bwd=1){
        
#   cat('\nrunning .v.gz')
  if(!missing(width) & !missing(height)) figdim <- c(width, height)

  #     obj <- subset(obj, substr(obj, nchar(obj)-2, nchar(obj)) == ".gz") # select binary files of which area is known
  file_def <- name_split(obj)
  if(missing(param)) param <- file_def$parameter
  
  parameter_definitions <- NULL
  rm(parameter_definitions)
  data('parameter_definitions',envir=environment())
  
  if(!any(parameter_definitions$param %in% param)) stop('error in .v.gz.r: Parameter definition is missing or not matching parameter list. Please select valid parameter label\n',
                                                        paste(paste(parameter_definitions$param,"\t",parameter_definitions$name1),collapse='\n'))
  param_def <- parameter_definitions[as.character(parameter_definitions$param) == param,]
  if(!missing(Log)) param_def$log <- Log
  if(nrow(param_def) == 0) stop('parameter not found, please check')
  
  if(missing(v_area)){v_area <- file_def$area}else{v_area <- v_area}
  b <- readbin(obj,area=v_area,Raster=T) # read binary data file
  
  outfile.name <- obj
  file_def2 <- file_def; file_def2$area <- v_area; outfile <- name_join(file_def2)
  outfile.name <- paste0(substr(outfile,1,nchar(outfile)-3),suffix)
  
  r <- regions(v_area)
  if(!missing(cbx)) r$cbx <- cbx
  if(!missing(cby)) r$cbx <- cby
  if(missing(grid.res)) grid.res <- .get.grid.res(r)
  if(!missing(figdim)) r$figdim <- figdim
  
 .v.plot(b=b, minv=minv,maxv=maxv, adaptive.vals=adaptive.vals, replace.na=replace.na, main=main, cb.title=cb.title, cb.xlab=cb.xlab, pal=pal,
         sidelabels=sidelabels, Ylab=Ylab, axeslabels=axeslabels, ticklabels=ticklabels, cex.lab=cex.lab, cex.ticks=cex.ticks, steps=steps,
         subplot=subplot, xpos=xpos, Save=Save, plotfolder=plotfolder, plotname=plotname, fileformat=fileformat, # suffix no longer required
         param=param, param_def=param_def, file_def=file_def, r=r, outfile.name=outfile.name, # set within this procedure
         v_area=v_area, v_image=v_image, v_contour=v_contour, contour.labels=contour.labels, levels=levels, v_arrows=v_arrows, scale_arrow=scale_arrow,
         fill=fill, col=col, border=border, grid=grid, grid.res=grid.res, bwd=bwd) # further arguments passed to plotmap
}