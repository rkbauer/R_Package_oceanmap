\name{get.bathy}
\alias{get.bathy}
\title{
Returns bathymetric data from the NOAA server as RasterLayer, given coordinate bounds and resolution.
}

\description{
Returns bathymetric data from the NOAA server as RasterLayer, given coordinate bounds and resolution.
}

\usage{
get.bathy(v_area, lon, lat, resolution=4, keep=F ,
          savename.bathy, folder.bathy, visualize=T, ...)
}

\arguments{
\item{v_area}{
  character string identifying the region that should be plotted, or in case of \code{x == 'bathy'}, also a Raster* or Extent object. If missing, region is derived from the \code{'.gz'}-filename. 
  See \link{region_definitions} for area definitions and use \link{add.region} to add new regions.
}

 \item{lon}{
Vector returning longitude coordinates of the area to be plotted, only valable for \code{x == 'bathy'}.
}
  \item{lat}{
Vector returning latitude coordinates of the area to be plotted, only valable for \code{x == 'bathy'}.
}

  \item{resolution}{
resolution of the bathymetric grid, in minutes (default is 4).
}

  \item{keep}{
whether to write the data downloaded from NOAA into a file (default is FALSE).
}

  \item{savename.bathy}{
savename for the bathymetric data file, if not specified set to type 'bathy_lon-lat_res.resolution.dat' or 'bathy_v_area_res.resolution.dat'.
}

  \item{folder.bathy}{
directory where bathymetric data should be saved (default is current working directory).
}

  \item{visualize}{
whether the bathymetric data should be plotted instantly.
}

  \item{...}{
additional arguments to be passed to \link{v}, used if \code{visualize} is set \code{TRUE}.
}
}
\seealso{\link{v}, \link{add.region}, \link{region_definitions}, \link{regions}, \link{writebin}, \link{get.bathy}} 

\author{
Robert K. Bauer
}

\keyword{bathymetry}
\keyword{image plot}


\examples{
## Example 1: plot bathymetry using a v_area-keyword
#get.bathy("lion",res=4, keep=T) # can take some time, requires server connection!
#get.bathy("lion",res=1, keep=T,visualize=F)


## Example 2: plot bathymetry of the Baltic Sea defined by longitude and latidtue coordinates
lon <- c(9, 31)
lat <- c(53.5, 66)
#get.bathy(lon=lon,lat=lat,visualize=T,main="Baltic Sea")


## Example 3: plot landmask of the Baltic Sea defined by an extent- or raster-object
library('raster')
ext <- extent(lon,lat)
#get.bathy(ext,visualize=T,main="Baltic Sea",res=4,levels=200, steps=200) # extent-object

}
