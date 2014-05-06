inst.pkg <- function(package){
  package <- as.character(substitute(package))
    if(!is.element(package, installed.packages()[,1])) install.packages(package)
    l <- library(package,character.only = T)
  return(l)
}
