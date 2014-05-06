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
owd <- getwd()
setwd(system.file("test_files", package="satmap"))
nfiles <- Sys.glob('*.nc') # load sample-'.nc'-files

nc2raster(nfiles[1],"Conc",layer=1) # RasterLayer
nc2raster(nfiles[1],"Conc",layer=1:4) # RasterStack

library('ncdf4')
nc <- nc_open(nfiles[1])
nc2raster(nc,"Conc",layer=1:4) # RasterStack

## ordinary raster call
raster(nfiles[1])
brick(nfiles[1],band=c(1,4))

setwd(owd)
}