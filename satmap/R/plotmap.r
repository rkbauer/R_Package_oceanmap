# last update: 18.sept.2013
plotmap <- function(region=v_area,lon,lat,add=F,axeslabels=T,ticklabels=T,grid=T,cex.lab=0.8,cex.ticks=0.8,grid.res,main,col="grey",border='black',fill=T,bwd=1,v_area){
  #   library("R.utils")
  #   sourceDirectory("~/Dropbox/Ifremer/scripts/funct") # load other functions
  #inst.pkg("maps") # needed to provide landmask
  ##inst.pkg("mapdata") # needed to provide fine resolution landmask "worldHires"
  ##inst.pkg("maptools") # needed to provide fine resolution landmask "worldHires"
  #inst.pkg("grDevices") # needed for png file creation
  #inst.pkg('raster')
  ##inst.pkg("Cairo") # needed for png file creation on server
  data('worldHiresMapEnv',envir=environment())
  if(!missing(region)){ #' if region information is given
    if(class(region) == 'character') {
      r <- regions(region) # get regions defintions (extent and name)
      if(missing(grid.res)) grid.res <- r$grid.res[1]
    }else{
      if(grepl('Raster', class(region)) | grepl('Extent', class(region))){
        extent.vector <- as.vector(t(bbox(extent(region))))
        r <- data.frame(xlim=extent.vector[1:2],ylim=extent.vector[3:4])
      }else{
        if(missing(lon) | missing(lat)) stop('error in plotmap: provide region information as name or lon, lat data')
      }
    }
  }
  if(!missing(lon) & !missing(lat))  r <- data.frame(xlim=range(lon),ylim=range(lat))
  if(missing(grid.res)) grid.res <- .get.grid.res(r)
  
  if(add == F) plot(10000,10000,type="p",cex=0,axes=F,xlim=r$xlim,ylim=r$ylim,xlab="",ylab="",asp=1) # l functions sets right axes limits, neccessary for maps
  
  par(cex.axis=cex.ticks)
  if(!fill) col <- NA
  map(database="worldHires", fill=T, col=col,xlim=r$xlim,ylim=r$ylim,add=T,resolution=0,border=border)
  #  # overplot landmask outside plotting region
  rect(r$xlim[1],-400,-400,400,col="white",border="white",xpd=T) # xleft, ybottom, xright, ytop
  rect(r$xlim[2],-400,400,400,col="white",border="white",xpd=T)
  rect(-400,r$ylim[1],400,-400,col="white",border="white",xpd=T)
  rect(-400,r$ylim[2],400,+400,col="white",border="white",xpd=T)
  
  x <- c(ceiling(r$xlim[1]/grid.res)*grid.res, floor(r$xlim[2]/grid.res)*grid.res)
  y <- floor(r$ylim/grid.res)*grid.res
  xlabels <- seq(x[1],x[2],grid.res)
  xlabels <- xlabels[xlabels >= r$xlim[1] & xlabels <= r$xlim[2]]
  ylabels <- seq(y[1],y[2],grid.res)
  ylabels <- ylabels[ylabels >= r$ylim[1] & ylabels <= r$ylim[2]]
  EW <- rep(" E",length(xlabels))
  EW[xlabels < 0] <- " W"                   
  NS <- rep(" N",length(ylabels))
  NS[ylabels < 0] <- " S"
  
  if(length(ticklabels) == 1) ticklabels <- rep(ticklabels,2)
  XLabels <- switch(ticklabels[1]+1,rep('',length(xlabels)),parse(text = paste(abs(xlabels)," *degree ~",EW,sep="")))
  YLabels <- switch(ticklabels[2]+1,rep('',length(ylabels)),parse(text = paste(abs(ylabels)," *degree ~",NS,sep="")))
  axis(1,pos=r$ylim[1],at=xlabels,labels=XLabels)
  axis(2,pos=r$xlim[1],at=ylabels,labels=YLabels)
  axis(side=3,pos=r$ylim[2],at=xlabels,labels=F,tick=T)
  axis(side=4,pos=r$xlim[2],at=ylabels,labels=F,tick=T)
  
  par(cex.axis=cex.lab)
  if(length(axeslabels) == 1) axeslabels <- rep(axeslabels,2)
  if(axeslabels[1]) axis(1,pos=r$ylim[1],at=mean(r$xlim),labels="longitude",tcl=0,mgp=c(0,2,0)) # parse(text = paste("longitude [ *degree ~ E]",sep=""))
  if(axeslabels[2]) axis(2,pos=r$xlim[1],at=mean(r$ylim),labels="latitude",tcl=0,mgp=c(0,2.5,0),xpd=T) # parse(text = paste("longitude [ *degree ~ N]",sep=""))
  
  ..boundaries(r$xlim,r$ylim,grid.res,bwd=bwd) # plot ..boundaries
  if(grid == T) .grid2(r$xlim,r$ylim,grid.res,"black") # plot grid
  
  if(!missing(main)) title(main)
}

..boundaries <- function (xrange,yrange, grid.res = 10, col = "lightgray", lwd = par("lwd"),bwd) 
{
  f <- bwd*min(diff(xrange),diff(yrange))/125
  xrange <- c(xrange[1]-f,xrange[2]+f)
  yrange <- c(yrange[1]-f,yrange[2]+f)
  sf = grid.res/2
  xmin <- ceiling(xrange[1]/sf)*sf # round up
  xmax <- floor(xrange[2]/sf)*sf
  ymin <- ceiling(yrange[1]/sf)*sf
  ymax <- floor(yrange[2]/sf)*sf
  color <- c("white","black")
  x1 <- c(xrange[1],seq(xmin,xmax,sf))
  x2 <- c(seq(xmin,xmax,sf),xrange[2])
  
  for (x in 1:length(x1))
  {
    rect(x1[x],yrange[1],x2[x],yrange[1]+f,col=color[x%%2+1],xpd=T)
    rect(x1[x],yrange[2],x2[x],yrange[2]-f,col=color[x%%2+1],xpd=T)
  }
  y1 <- c(yrange[1],seq(ymin,ymax,sf))
  y2 <- c(seq(ymin,ymax,sf),yrange[2])
  for (y in 1:length(y1))
  {
    rect(xrange[1],y1[y],xrange[1]+f,y2[y],col=color[y%%2+1],xpd=T)
    rect(xrange[2]-f,y1[y],xrange[2],y2[y],col=color[y%%2+1],xpd=T)
  }
}

.grid2 <- function (xrange,yrange, grid.res = 10, col = "lightgray", lty = "dotted", lwd = par("lwd")) 
{
  xmin <- ceiling(xrange[1]/grid.res)*grid.res
  xmax <- floor(xrange[2]/grid.res)*grid.res
  ymin <- ceiling(yrange[1]/grid.res)*grid.res
  ymax <- floor(yrange[2]/grid.res)*grid.res
  
  for (x in seq(xmin,xmax,grid.res))
  {
    lines(c(x,x),yrange,col = col, lty = lty, lwd = lwd)
  }
  
  for (y in seq(ymin,ymax,grid.res))
  {
    lines(xrange,c(y,y),col = col, lty = lty, lwd = lwd)
  }
}

# l <- function(lim)
# {
#   lim2 <- lim
#   lim2[1] <- lim[1]+(diff(lim)-(diff(lim)/1.08))/2
#   lim2[2] <- lim[2]-(diff(lim)-(diff(lim)/1.08))/2
#   return(lim2)
# }