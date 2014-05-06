regions <- function(label) {
#   region_definitions <-read.table('/home/robert/Dropbox/Ifremer/scripts/R/satmap/regions.csv', header=T, sep=";")
#   head(region_definitions)
#   region_definitions$figxdim[region_definitions$label =='survey'] <- 5.053054
#   region_definitions$figydim[region_definitions$label =='survey'] <- 4.992951
#   write.table(region_definitions,'/home/robert/Dropbox/Ifremer/scripts/R/satmap/regions.csv', sep=";",row.names=F)
#   region_definitions <- data.frame(region_definitions[,1:13],gradient="x",oticks='b',region_definitions[,14:16])
# save(region_definitions, file='/home/robert/Dropbox/Ifremer/scripts/R/satmap/regions.RData')
#   load(file='/home/robert/Dropbox/Ifremer/scripts/R/satmap/region_definitions.RData')
  region_definitions <- NULL
  rm(region_definitions)
  data('region_definitions',envir=environment())
  
  i <- which(region_definitions$label == label)
  if (length(i) < 1) stop("error in regions.r: Selected region not found! Please select valid region label:\n",paste(paste(region_definitions[,1],"\t",region_definitions[,2]),collapse='\n'))
  
  rlon <- c(region_definitions$lonw[i],region_definitions$lone[i])
  rlat <- c(region_definitions$lats[i],region_definitions$latn[i])
  rdim <- c(region_definitions$ncol[i],region_definitions$nrow[i])
  rname <- as.character(region_definitions$name[i])
  rx <- c(region_definitions$cbx1[i],region_definitions$cbx2[i]) # x-ccordination of colorbar
  ry <- c(region_definitions$cby1[i],region_definitions$cby2[i]) # y-ccordination of colorbar
  rfigdim <- c(region_definitions$figxdim[i],region_definitions$figydim[i])
  align <- c(as.character(region_definitions$gradient[i]),as.character(region_definitions$oticks[i]))
  return(data.frame(xlim=rlon, ylim=rlat, dim=rdim, name=rname,cbx=rx,cby=ry,align=align,figdim=rfigdim,grid.res=region_definitions$grid.res[i]))
}


