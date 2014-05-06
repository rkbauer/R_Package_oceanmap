add.region <- function(add, add.px, cbx, cby, figdim, lib.folder, widget=T, backup=F, backup.folder='.', backup.name, restore=F){ #,locat.colbar=T)
  
  if(missing(lib.folder)) lib.folder <- .libPaths()
  lib.folder.satmap <- paste0(lib.folder,"/satmap")
  lib.folder.satmap <- lib.folder.satmap[file.exists(lib.folder.satmap)]
  if(length(lib.folder.satmap) == 0) stop("error in add.region: could not find satmap package. please check library path.")
  if(length(lib.folder.satmap) > 1){
    Rcheck <- grepl('satmap.Rcheck', lib.folder.satmap)
    if(any(Rcheck)){
      lib.folder.satmap <- lib.folder.satmap[which(Rcheck)]
    }else{
      stop("error in add.region: satmap package found in multiple R libraries. please define lib.folder.")
    }
  }
  region_definitions.path <- paste0(lib.folder.satmap,"/data/region_definitions.rda")
  if(file.exists(region_definitions.path)) load(region_definitions.path)
  
  ##' region_definitions data set is autoloaded with the package
  #   load("/home/robert/Dropbox/Ifremer/scripts/R/satmap/data/region_definitions.rda")
  #   save(region_definitions,file="/home/robert/Dropbox/Ifremer/scripts/R/satmap/data/region_definitions.bkp.rda")
  data('region_definitions',envir=environment())
  ids <- nrow(region_definitions)
  
  if(backup){
    if(missing(backup.name)) backup.name <- paste0(.check.folder(backup.folder),"region_definitions.bkp.",format(Sys.time(),format="%Y%m%d"),".rda")
    cat(paste("backup of region_definitions saved in:", backup.name))
    save(region_definitions,file=backup.name)
  }else{
    
    if(restore){
      if(missing(backup.name)) backup.name <- paste0(lib.folder.satmap,"/data/region_definitions.bkp.rda")
      system(paste('cp', backup.name, region_definitions.path))
      cat(paste("restoring of region_definitions from:", backup.name))
    }else{
      
      if(!missing(add.px)){
        id <- which(as.character(region_definitions$label) == as.character(add.px$label))
        if(length(id) == 0) stop("selected region not found! please select valid region label:\n",paste(paste(region_definitions[,1],"\t",region_definitions[,2]),collapse='\n'))
        if(!is.numeric(add.px$ncol)| !is.numeric(add.px$nrow)) stop('entered values for add.px$nrow or add.px$ncol are not numeric, please revise!')
        #' ncol
        if(!is.na(region_definitions$ncol[id])){
          cat(paste("'ncol'-value for region", add.px$label), 'already set to', region_definitions$ncol[id])
          enter <- readline(paste("press ENTER if you wish to overwrite it with", add.px$ncol, 'or any other button to keep the existing value'))
          if(enter == '') region_definitions$ncol[id] <- add.px$ncol
        }else{
          region_definitions$ncol[id] <- add.px$ncol
        }
        
        #' nrow
        if(!is.na(region_definitions$nrow[id])){
          cat(paste("'nrow'-value for region", add.px$label), 'already set to', region_definitions$nrow[id])
          enter <- readline(paste("press ENTER if you wish to overwrite it with", add.px$nrow, 'or any other button to keep the existing value'))
          if(enter == '') region_definitions$nrow[id] <- add.px$nrow
        }else{
          region_definitions$nrow[id] <- add.px$nrow
        }
        region_definitions$px[id] <- region_definitions$ncol[id]*region_definitions$nrow[id]
        new.region <- add.px$label
        
        cat(paste0('\nnew region ',new.region), 'was modified to:')
        print(region_definitions[id,])
        
        
      }else{
        if(missing(add)) add <- c()
        if(!(length(add) == length(region_definitions))){
          v_area <- c()
          if(grepl('Raster', class(add)) | grepl('Extent', class(add))){
            v_area <- as.vector(t(bbox(extent(add))))
          }
          add.lon <- v_area[1:2]
          add.lat <- v_area[c(4,3)]
          if(widget){
            add <- region_definitions[ids+1,]
            longnames <- c("keyword",
                           "long name",
                           "northern most latitude",
                           "southern most latitude",
                           "western most longitude",
                           "eastern most longitude")
            i <- 1
            while (i <= 6){
              if(i %in% 3:4 & length(add.lat) > 0){
                val <- add.lat[which(3:4 %in% i)]
              }else{
                if(i %in% 5:6 & length(add.lon) > 0){
                  val <- add.lon[which(5:6 %in% i)]
                }else{
                  val <- readline(paste0("\nPlease define the ",longnames[i], " of the new region, coded as '",names(region_definitions)[i],"':"))
                  cat(val)
                }
              }
              if(i %in% 1:2) add[[i]] <- val
              if(i %in% 3:16) {
                add[[i]] <- as.numeric(val)
                if(!is.finite(unlist(c(add)[i]))){
                  warning(paste0("error in add.region: added value '",val,"' for '", names(region_definitions)[i], "' is not numeric. please check."),immediate.=T)
                  i <- i-1
                }
              }
              i <- i+1
            }
            
            cb <- cust.colorbar(lon=c(add$lonw, add$lone),lat=c(add$lats,add$latn),cbx=cby,cby=cby,figdim=figdim,force.figdim.widget=T)
            add$cbx1 <- cb$cbx[1]
            add$cbx2 <- cb$cbx[2]
            add$cby1 <- cb$cby[1]
            add$cby2 <- cb$cby[2]
            add$gradient <- cb$align[1]
            add$oticks <- cb$align[2]
            add$figxdim <- cb$figdim[1]
            add$figydim <- cb$figdim[2]
            
            add$grid.res <- as.numeric(readline("\nPlease enter default grid resolution."))
          }
          cat("\nEntry section finished.\n")
          #     cat(region_definitions)
        }
        print(add)
        
        if(length(add)!= length(region_definitions)) stop("error in add.region: added data is incomplete. please check and try again.")
        for(i in c(10:13,16:18)) if(any(!is.numeric(unlist(c(add)[i])))| any(!is.finite(unlist(c(add)[i])))) stop(paste0("error in add.region: added value '",as.vector(add)[i],"' for '", names(region_definitions)[i], "' is not numeric. please change."))
        
        new.region <- unlist(c(add[1])) # return label
        if(any(as.character(region_definitions$label) == new.region)) stop("error in add.region: region label '",new.region,"' already reserved. please change.")
        
        region_definitions <- rbind(region_definitions,add)
        row.names(region_definitions) <- 1:nrow(region_definitions)
        
        enter <- readline("\nPress <Enter> to save region configuration.")
        if(enter != "") stop("Operation stopped by user")
        cat(paste0('\nnew region added under label: ',new.region))
      }
      save(region_definitions,file=region_definitions.path)
    }
  }
}
