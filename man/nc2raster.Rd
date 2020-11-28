\name{nc2raster}
\alias{nc2raster}
\title{
Convert Raster layer to a matrix or array}
\description{
\code{nc2raster} converts a netcdf-file ('.nc'-file) or \code{ncdf4}-object to a Raster* object, setting the time variable as layer name.
}
\usage{
nc2raster(nc, varname, t=layer, lonname="lon", latname="lat", 
    layer, date=T)
}
\arguments{
\item{nc}{character string indicating the filepath to a netcdf-file ('.nc'-file), or a \code{ncdf4}-object.
}

 \item{varname}{
character string indicating the name of the netcdf-variable to be selected.
}

 \item{lonname}{
 character string indicating the name of the longitude-variable of \code{ncdf4}-objects and \code{'.nc'}-files to plot (default is \code{'lon'})
}
 
 \item{latname}{
 character string indicating the name of the latitude-variable of \code{ncdf4}-objects and \code{'.nc'}-files to plot (default is \code{'lat'})
}

 \item{layer, t}{
layer/time stemp to select in multi-layer files.
}

 \item{date}{
whether the layer names should be set to the date of the ncdf-file layer (default is \code{TRUE}, format is 'X\%Y\%m\%d').
}
}

\author{
Robert K. Bauer
}

\value{
RasterLayer or RasterStack
}

\examples{
path <- system.file("test_files", package="oceanmap")
nfiles <- Sys.glob(paste0(path,'/*.nc'))[1] # load sample-'.nc'-files

nc2raster(nfiles[1],"Conc",layer=1) # RasterLayer
nc2raster(nfiles[1],"Conc",layer=1:4) # RasterStack

library('ncdf4')
nc <- nc_open(nfiles[1])
nc2raster(nc,"Conc",layer=1:4) # RasterStack

###### load & plot sample netcdf-file ('.nc'-file)

### option a) load netcdf-file with ncdf4-package and plot it
library('ncdf4')
ncdf <- nc_open(nfiles[1])
print(ncdf)
v(obj = ncdf, cbpos="r")

### option b) load and plot netcdf-file as RasterStack object
nc <- nc2raster(nfiles[1])
v(nc,cbpos="r") # plot RasterStack object
v(nfiles[1], cbpos="r",replace.na=TRUE) # plot directly netcdf-file

### option c) plot netcdf-file directly
v(nfiles[1], cbpos="r") # plot RasterStack object

###### plot multiple layers:
par(mfrow=c(2,2))
v(nfiles[1], t=1:4, cbpos="r", replace.na=TRUE, subplot = TRUE)

}