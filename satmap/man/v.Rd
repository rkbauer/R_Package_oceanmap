\name{v}
\alias{v}
\alias{v.plot}
\alias{v.bathy}
\alias{v.gz}
\alias{v.raster}
\alias{v,bathy-method}
\alias{v,RasterBrick-method}
\alias{v,RasterLayer-method}
\alias{v,RasterStack-method}
\alias{v,character-method}
\alias{v,gz-method}
\alias{v,nc-method}
\alias{v,ncdf4-method}
  
  
\title{
Ploting geographic data}
\description{
Plots geographic data. Valid input data are objects of class 'Raster' ('RasterLayer', 'RasterStack' or 'RasterBrick'), 'ncdf4' or a character strings indicating \code{'bathy'}metric data, 'gz'- or '.nc-files. See also \link{name_split} for further information on \code{'.gz'}-file nomenclature. 
}

\usage{

\S4method{v}{bathy}(obj, v_area, lon, lat, resolution=4, keep=F, 
  savename.bathy, folder.bathy=".", adaptive.vals=T, cb.title, ...)

\S4method{v}{nc}(obj, varname, t=1, adaptive.vals=T, dates, 
  cb.xlab=varname, lonname="lon", latname='lat',...)

\S4method{v}{ncdf4}(obj, varname, t=1, adaptive.vals=T, dates, 
  cb.xlab=varname, lonname="lon", latname='lat',...)

\S4method{v}{RasterLayer}(obj, varname, t=1, ...)

\S4method{v}{RasterBrick}(obj, varname, t=1, ...)

\S4method{v}{RasterStack}(obj, varname, t=1, ...)

\S4method{v}{gz}(obj, v_area, adaptive.vals=F, ...)
}

\arguments{
 \item{obj}{
object of class 'Raster' ('RasterLayer', 'RasterStack' or 'RasterBrick'), '\code{ncdf4}' or a character string indicating , \code{'bathy'}metric data, \code{'.gz'}- or \code{'.nc'}-files to plot. 
}

\item{v_area}{
  character string identifying the region that should be plotted, or in case of \code{obj == 'bathy'}, also a Raster* or Extent object. If missing, region is derived from the \code{'.gz'}-filename. 
  See \link{region_definitions} for area definitions and use \link{add.region} to add new regions.
}

 \item{adaptive.vals}{
sets minimum and maximum z-value according to the \code{'.gz'}-files value range. (\bold{ATTENTION!} \code{minv} and \code{maxv} are disregarded if set!).
(default is \code{TRUE} for non-\code{'.gz'}-files. If \code{FALSE} or not set, default value from the \link{parameter_definitions}-dataset will be applied according to the \code{param}-value.
}

 \item{t}{
layer/time stemp to select in multi-layer files.
}

 \item{dates}{
vector of type 'character' indicating dates per layer, used to define the title of the colorbar. Argument is omitted for \code{'.gz'}-files but date-information is derived from the filename. For \code{'.nc'}-files or 'ncdf4'-objects, date information is derived from the time-vector. For \link{raster}-objects the layer name is applied.
}

 \item{varname}{
character string indicating the name of the variable to plot. For \code{'.nc'}-files or 'ncdf4'-objects, this name must correspond to a variable name defined in the file/object. Sets also colorbar-title for non-\code{'.gz'}-files if \code{cb.xlab} is missing.
}

 \item{cb.title}{
character string indicating the title of the colorbar (default is set to date information/empty string if date information is missing.)
}

 \item{cb.xlab}{
character string indicating the x-axis label of the colorbar. If not defined, it will be set to \code{varname} for \link{raster}, \code{ncdf4}-objects and \code{'.nc'}-files or for \code{'.gz'}-files to a predefined title in the \link{parameter_definitions}-dataset according to the \code{param}-value.
}

 \item{lonname}{
 character string indicating the name of the longitude-variable of \code{ncdf4}-objects and \code{'.nc'}-files to plot (default is \code{'lon'})
}
 
 \item{latname}{
 character string indicating the name of the latitude-variable of \code{ncdf4}-objects and \code{'.nc'}-files to plot (default is \code{'lat'})
}

 \item{lon}{
Vector returning longitude coordinates of the area to be plotted, only valable for \code{obj == 'bathy'}.
}
  \item{lat}{
Vector returning latitude coordinates of the area to be plotted, only valable for \code{obj == 'bathy'}.
}

  \item{resolution}{
resolution of the bathymetric grid, in minutes (default is 4), only valable for \code{obj == 'bathy'}.
}

  \item{keep}{
whether to write the data downloaded from NOAA into a file (default is FALSE), only valable for \code{obj == 'bathy'}.
}

  \item{savename.bathy}{
savename for the bathymetric data file, if not specified set to type 'bathy_lon-lat_res.resolution.dat' or 'bathy_v_area_res.resolution.dat', only valable for \code{obj == 'bathy'}.
}

  \item{folder.bathy}{
  directory where bathymetric data should be saved (default is current working directory), only valable for \code{obj == 'bathy'}.
}


 \item{\dots}{
additional arguments to be passed:
\describe{

\item{\code{region}}{
  see \code{v_area}.
}

\item{\code{minv, maxv}}{
  minimum and maximum z-value to be plotted. If not set, default value from the \link{parameter_definitions}-dataset will be applied. Argument is overwritten by \code{adaptive.vals} and \code{zlim}.
}

\item{\code{replace.na}}{
  whether missing values should be replaced by minimum values (default is \code{FALSE}.)
}

 \item{\code{param}}{
character string indicating the parameter name for the dataset treatment. See \code{parameter_definitions} for available parameters. For \code{'.gz'}-files, \code{param} is derived from the filename. For non-\code{'.gz'}-files this value is non-obligatory, but can replace the \code{varname}-argument and vise versa. See examples.
}

\item{\code{main}}{
  an overall title for the plot: see \link{title}.
}

\item{\code{cbx}}{
 the horizontal limits (x1, x2) of the colorbar. If missing and the value can not be reconstructed by the region information (e.g. \code{v_area}, \code{'.gz'}-file), the user will be asked for manual colorbar placement.
}

\item{\code{cby}}{
 the vertical limits (y1, y2) of the colorbar. If missing and the value can not be reconstructed by the region information (e.g. \code{v_area}, \code{'.gz'}-file), the user will be asked for manual colorbar placement.
}

\item{\code{pal}}{
  color map to be plotted (defualt is the 'jet'-colormap, or in case of \code{'.gz'}-files derived from the \link{parameter_definitions}-dataset. See \link{cmap} for available color maps and \link{parameter_definitions} for predefined colormaps for different parameters.)
}

\item{\code{steps}}{
number: step size between minimum and maximium colorbar ticks.
}

\item{\code{sidelabels}}{
  whether an additional y-axis label and title should be added to the plot device (default is \code{FALSE}). If \code{TRUE}, y-axis label is defined by \code{Ylab}, the additional title is derived from the \code{date}-information and gives the month information.
}

\item{\code{Ylab}}{
  an additional title for the y axis (default is date information), only used when \code{sidelabels} is set \code{TRUE}. Default value is year-information.
}

\item{\code{axeslabels}}{
  whether axeslabels should be shown (default is \code{TRUE}, set as 'longitude' and 'latitude')
}

\item{\code{subplot}}{
  whether \code{'.gz'}-file will be plotted as a sub plot to an existing plot device (default is \code{FALSE}; see: \link{par})
}

\item{\code{width, height}}{the width and height of the plotting window, in inches. For \code{'.gz'}-files, default values are derived from the \code{region}-name as indicated by the filename. See \link{region_definitions} for predescribed definitions and use \link{add.region} to add new region definitions.
}

\item{\code{figdim}}{
  numeric vector indicating the \code{width} and \code{height} of the plot device in inches. For \code{'.gz'}-files, default values are derived from the \code{region}-name as indicated by the filename. Value is overwritten if both, \code{width} and \code{height} are provided. See \link{region_definitions} for predescribed definitions and use \link{add.region} to add new region definitions.
}

\item{\code{xpos}}{
  integer: initial position of the top left corner of the figure window on the pc-screen, given in pixels. Negative values are from the opposite corner. (default is -1). Omitted if \code{Save} is set \code{TRUE}.
}

\item{\code{Save}}{
  whether the a plot device should be saved automatically as an image file of type \code{fileformat} in a folder specified by \code{plotfolder} (default is \code{FALSE})
}

\item{\code{plotfolder}}{
  directory where images should be saved (default is current working directory).
}

\item{\code{plotname}}{
the name of the output file(s). If not set, value will be derived from the provided file information (For \code{'.gz'}-files, default \code{plotname} is equal to the \code{'.gz'}-filename, replacing the \code{'.gz'}-fileformat-suffix with the defined image-\code{fileformat}.
}

\item{\code{fileformat}}{
  fielformat of image file to be saved (only \code{png} and \code{eps} are accepted; default is \code{png}).
}

\item{\code{suffix}}{
  suffix to be added to the image filename, before the filetype specification (e.g. '...suffix.png').
}

\item{\code{v_image}}{
  whether an image-plot should be plotted (default is \code{TRUE})
}

\item{\code{v_contour}}{
  whether \code{contour} lines should be plotted (default is \code{FALSE}). If \code{levels} are specified, \code{v_contour} is set \code{TRUE}.
}

\item{\code{levels}}{
  numeric vector of levels at which to draw contour lines.
}

\item{\code{contour.labels}}{ 
a vector giving the labels for the contour lines. By default levels are used as labels.
}

\item{\code{v_arrows}}{
  whether current or wind vectors should be plotted (default is \code{TRUE}; Argument is disregarded for non-\code{.gz}-files and omitted if non current or wind data-files are provided)
}

\item{\code{scale_arrow}}{
  scale factor needed for current and wind vector plots (default is 1; Argument is disregarded for non-\code{.gz}-files and omitted if no current or wind data-files are provided, indicated by the param-argument (valid \code{param}-definitions are: 'uz' and 'vz', for current data, 'wu' and 'wz' for wind data))
}

\item{\code{\dots}}{
  Additional arguments to be passed to \code{\link{plotmap}} (\code{bwd}, \code{fill}, \code{col}, \code{border}, \code{grid}, \code{grid.res}, \code{axeslabels}, \code{ticklabels}, \code{cex.lab}, \code{cex.ticks}).
}
}
}
}

\details{
\code{v} uses the \code{maps} and \code{maptools} functions to plot the landmask. See \code{\link{clim_plot}} for aligned plots of satallite-data climatologies.
}
\seealso{ \code{\link{clim_plot}, \link{readbin}, \link{name_split}, \link{regions}, \link{plotmap}, \link{v}}}


\author{
Robert K. Bauer
}

\keyword{bathymetry}
\keyword{image plot}

\references{
Bauer, R. K., Stepputtis, D., Grawe, U., Zimmermann, C., and Hammer, C. 2013. Wind-induced variability in coastal larval retention areas: a case study on Western Baltic spring-spawning herring. Fisheries Oceanography, 22: 388-399.
}

\examples{

## Example 1: plot bathymetry using a v_area-keyword
## requires server connection!
# par(mfrow=c(2,1))
# v("bathy","lion",res=4, keep=TRUE,border='grey',subplot=TRUE, 
#   main='Gulf of Lions bathymetry',cb.title="resolution 4 min")

# v("bathy","lion",res=1, keep=TRUE,border='grey',subplot=TRUE,
#   cb.title="resolution 1 min")  # can take some time depending on server connection!
  

## Example 2: plot bathymetry of the Baltic Sea defined by longitude and latidtue coordinates
## requires server connection!
lon <- c(9, 31)
lat <- c(53.5, 66)
#v("bathy",lon=lon,lat=lat,main="Baltic Sea")


## Example 3: plot landmask of the Baltic Sea defined by an extent- or raster-object
## requires server connection!
library('raster')
ext <- extent(lon,lat)
# v("bathy",ext,main="Baltic Sea",res=4,levels=200, steps=200) # extent-object


## Example 4: plot '.gz'-files, following default plot-procedure
owd <- getwd()
setwd(system.file("test_files", package="satmap"))
check_gzfiles() # return file summary-table
gz.files <- Sys.glob('*.gz') # load sample-'.gz'-files
v(gz.files[1:3])
v(gz.files[3],bwd=2)

## Example 5: plot climatologies from '.gz'-files 
##            (ATTENTION: not working for non-'gz'-files, requiring ImageMagick)
clim_plot('*1s*.gz',bwd=0.7,adaptive.vals=TRUE,plotname="seasonal_climatology.png")

## Example 6: plot subregion of gz-files as subplots
graphics.off()
par(mfrow=c(2,1))
v(gz.files[1:2],v_area='lion',subplot=TRUE) # run ?region_definitions to see predefined regions


## Example 7: plot subregion of gz-file with different color maps
data('cmap')
par(mfrow=c(3,4))
for(i in 1:11) v(gz.files[2],v_area='lion',subplot=TRUE,pal=names(cmap)[i],
                 adaptive.vals=TRUE,main=names(cmap)[i])


## Example 8: plot subregion of raster file

# all manual:
obj <- readbin(gz.files[2],area='lion')
dev.new()
ticks <- seq(20,30,5)
image(obj,zlim=range(ticks),col=cmap$jet)
plotmap('lion',add=TRUE) # add landmask
# set.colorbar(ticks=ticks,cb.title='cb.title',cb.xlab='cb.xlab')

# using v, reconstructing region information
v(obj,varname="sst2",cb.title='cb.title',cb.xlab='cb.xlab')

# using v for another subregion
ncorse <- crop(obj,extent(6,9,40,42))
# v(ncorse,grid.res=1)
# v(ncorse,zlim=c(20,30),cbx=c(8.3,8.9),cby=c(40.7,40.8)) # skipping colorbar widget


## Example 9: Add region by supplying raster-object, colorbar positions and running the widget
#add.region(ncorse,cbx=c(8.3,8.9),cby=c(40.7,40.8))


## Example 10: plot netcdf-files ('.nc'-files)
nfiles <- Sys.glob('*.nc') # load sample-'.nc'-files
head(nfiles)

## plot herring larval dispersal from Bauer et al. (2013)
# par(mfrow=c(2,2))
# v(nfiles[1], subplot=TRUE, t=1:4,minv=0, maxv=1000, adaptive.vals=FALSE, replace.na=TRUE)
# par(new=TRUE,mfrow=c(1,1))
# empty.plot(main='herring larval dispersal in the Greifswald lagoon, Germany')
# mtext('see Bauer et al. (2013) as reference')

# plot bathymetric data (obtained from the Leibniz Institute for Baltic Sea Research Warnemuende)
# v(nfiles[2],varname='bathymetry') # following default plot-procedure
# v(nfiles[2],varname='bathymetry',pal='haxbyrev',Log=TRUE, cb.xlab='depth [log m]',levels=50)
}
