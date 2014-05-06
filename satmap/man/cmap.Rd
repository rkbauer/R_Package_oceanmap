\name{cmap}
\alias{cmap}
\docType{data}
\title{
color maps 
}
\description{
list holding different color maps that can be used in image plots (see: \link{image}, \link{image.plot}s, \link{v}, \link{clim_plot})

available color maps are: \code{ano}, \code{bathy}, \code{blue}, \code{chla}, \code{green}, \code{haxby}, \code{jet} (obtained from matlab), \code{rainbow}, \code{red}, \code{sst} and \code{haxbyrev}.
}
\usage{data(cmap)}
\format{
  list
}

%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(cmap)
names(cmap)

## simple example of the \link{image}-function
x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano, col = terrain.colors(100))
image(x, y, volcano, col = cmap$jet) # jet color map
image(x, y, volcano, col = cmap$haxby) # haxby color map
image(x, y, volcano, col = cmap$chla) # chlorophyll color map
image(x, y, volcano, col = cmap$sst) # sst color map
}
\keyword{colorbar}

\author{
Robert K. Bauer
}
