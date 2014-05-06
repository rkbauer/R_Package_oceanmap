delete.region <- function(region,lib.folder,restore=F){
  
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
  file.exists(region_definitions.path)
  
  if(restore){
    cat(paste0("\nGoing to restore original region_definitions-file"))
    enter <- readline("\nPress <Enter> to continue")
    if(enter != "") stop("Operation stopped by user")
    env <- new.env()
    
    region_definitions.bkp <- NULL
    rm(region_definitions.bkp)
    data('region_definitions.bkp',envir=environment())    
    region_definitions <- region_definitions.bkp
  }else{
    region_definitions <- NULL
    rm(region_definitions)
    data('region_definitions',envir=environment())
    
    id <- which(region_definitions$label == region)
    if (length(id) < 1) stop("error in delete.region: selected region not found! please select valid region label:\n",paste(paste(region_definitions[,1],"\t",region_definitions[,2]),collapse='\n'))
    cat(paste0("\nGoing to delete region '",region,"':\n"))
    print(region_definitions[id,])
    enter <- readline("\nPress <Enter> to continue")
    if(enter != "") stop("Operation stopped by user")
    region_definitions <- region_definitions[-id,]
  }
  save(region_definitions,file=region_definitions.path)
}