cust.colorbar <- function(v_area,lon,lat,cbx,cby,figdim,input.mode,force.figdim.widget=F,xpos){
  open.dev <- F
  if(!missing(v_area)){ #' if region information is given
    if(class(v_area) == 'character') {
      r <- regions(v_area) # get regions defintions (extent and name)
    }else{
      if(class(v_area) == 'Extent') { 
        extent.vector <- as.vector(t(bbox(v_area)))
        r <- data.frame(xlim=extent.vector[1:2],ylim=extent.vector[3:4])
        
      }else{
        if(missing(lon) | missing(lat)) stop('error in plotmap: provide region information as name or lon, lat data')
      }
    }
    lon <- r$xlim
    lat <- r$ylim
  }
  
  grads <- c(abs(diff(lon)),abs(diff(lat)))
  grads2 <- grads/min(grads)
  
  add <- list()
  
  if(missing(cbx) | missing(cby)){
    widget.on <- T
    enter <- 'r'
    while(enter == 'r'){
      suppressWarnings(dev.new(width=6*grads2[1],height=6*grads2[2],xpos=xpos))
      open.dev <- T
      par(mar=c(7*grads2[2], 2*grads2[1], 1*grads2[2], 6*grads2[1])) # y1,x1,y2,x2
      plotmap(lon=lon,lat=lat)
      
      if(missing(input.mode)) input.mode <- readline("\nPlease type 'i' if you want to perform colorbar placement by hand, 'v' for a vertical or 'h' for a horizontal placement right or below the plot.")
      
      if(input.mode == 'v'){
        add$cbx <- c(lon[2]+0.08*min(grads),lon[2]+0.12*min(grads))
        add$cby <- lat
      }
      if(input.mode == 'h'){
        add$cbx <- lon
        add$cby <- range(c(lat[1]-0.22*min(grads),lat[1]-0.26*min(grads)))
      }
      
      if(input.mode == 'i'){
        cat("\nPlease select the lower left colorbar-position of the new region, coded as 'cbx1' and 'cby1':")
        p1 <- locator(n=1)
        points(p1,col='blue',pch=19)
        
        cat("\nPlease select the upper right colorbar-position of the new region, coded as 'cbx2' and 'cby2'.")
        #       cat("\nDon't select large cby2-value as procedure is tick-mark procedure is optimized for small cby2-cby1.")
        cat('\n(example colorbar based on selected points will be shown later)')
        p2 <- locator(n=1)
        points(p2,col='blue',pch=19)
        
        add$cbx <- range(c(p1$x,p2$x))
        add$cby <- range(c(p1$y,p2$y))
      }
      
      align <- set.colorbar(cbx=add$cbx,cby=add$cby,cb.xlab='cb.xlab',cb.title='cb.title')
      align <- c(align$gradient,align$oticks)
      enter <- readline("\nPress <Enter> to continue, 'r' to return editing colorbar position or any other key to abort operation")
    }
    cat(paste(paste("selected colorbar positions (can be entered also as argument!):\ncbx "),paste(add$cbx,collapse=" "), "\ncby ",paste(add$cby,collapse=" ")),'\n')
  }else{
    widget.on <- F
    add$cbx <- cbx
    add$cby <- cby
    grads <- abs(diff(cby))-abs(diff(cbx))    
    gradient <- c('x','y')[((grads > 0)+1)]
    if(gradient == 'x'){
      oticks <- "b"
    }else{
      oticks <- c('l','r')[((mean(cbx) > mean(lon))+1)]
    }
    align <- c(gradient,oticks)
  }
  enter <- ""
  if(missing(figdim) & !force.figdim.widget){
    figdim <- c(7,7)
  }else{
    if(missing(figdim)){
      if(force.figdim.widget & !widget.on){
        suppressWarnings(dev.new(width=6*grads2[1],height=6*grads2[2],xpos=xpos));
        open.dev <- T
        par(mar=c(7*grads2[2], 2*grads2[1], 1*grads2[2], 6*grads2[1])) # y1,x1,y2,x2
        plotmap(lon=lon,lat=lat)
      }
      enter <- readline("\nYou can resize the window to appropriate size. Try to avoid white space. Press <Enter> when done.")
      figdim <- dev.size()
    }
  }
  add$align <- align  
  add$figdim <- figdim
  
  if(open.dev) dev.off()
  
  if(enter != "") stop("Operation stopped by user")
  return(add)
}