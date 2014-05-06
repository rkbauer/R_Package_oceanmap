### function used to check types of available filesuffix in a directory

# folder <- '~/med4_avhrr_sst2_4km'
check_gzfiles <- function(sstring="*",folder=".",filetype=".gz")
{
  if(length(sstring) == 1){
    if(missing(folder) | folder != F){
      setwd(folder)
      #files <- unlist(system(paste("find . -maxdepth 0 -name '",sstring,"' -print0 |xargs -0 ls",sep=""),intern=T))
      files <- Sys.glob(sstring)
    }else{files <- sstring}
    #   files <- unlist(system(paste("ls ", sstring, sep=""),intern=TRUE))
    files <- files[files != "dummy.gz"]
  }else{
    files <- sstring
  }
  if(length(files) == 0)stop(paste0('ERROR: there is no file of the type ',sstring))

  files <- subset(files, substr(files, nchar(files)+1-nchar(filetype), nchar(files)) == filetype)
  objs <- matrix(as.character(unlist(strsplit(files,"\\_"))),ncol=7,byrow=T)
  
  for (i in 1:length(objs[,7]))
  {
    objs[i,7] <- substr(objs[i,7], 9,nchar(objs[i,7]))
  }
  objs <- data.frame(objs)
  objs <- data.frame(objs[,1:5],objs[,7])
  colnames(objs) <- c("region", "sat", "param", "res", "ts", "option")
  head(objs)
  objs <- cbind(objs,files=1)
  objs.agg <- aggregate(objs$files, by=as.list(objs[,1:6]), FUN=sum)
  colnames(objs.agg)[7] <- 'files'
  objs.agg$option <- unlist(strsplit(as.character(objs.agg$option),filetype))
  objs.agg$filetype <- filetype
  objs.agg
}