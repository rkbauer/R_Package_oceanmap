\name{set.colorbar}
\alias{set.colorbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adds colorbar to an extisting plot device
}
\description{
Adds colorbar to an extisting plot device. If position vectors are not provided, the user will be asked to define the colorbar placement by the mouse cursor.
}
\usage{
set.colorbar(cbx, cby, pal="jet", ticks=1:10, labels=ticks, 
             gradient, oticks, cb.title="", cb.xlab="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cbx}{
 the horizontal limits (x1, x2) of the colorbar. If missing, the user will be asked for manual colorbar placement.
}
  \item{cby}{
 the vertical limits (y1, y2) of the colorbar. If missing, the user will be asked for manual colorbar placement.
}
  \item{pal}{
color map to be plotted (default is \code{'jet'} for direct calls). See \link{cmap} for available color maps and \link{parameter_definitions} for predefined colormaps of different parameters (for internal function calls, e.g. \link{v}))
}
  \item{ticks}{
the points at which tick-marks are to be drawn (default is 1:10). Non-finite (infinite, NaN or NA) values are omitted.
}
  \item{labels}{
character or expression vector of labels to be placed at the tickpoints. (default equals \code{ticks}-values.)
}
  \item{gradient}{
whether to have a horizontal (x) or vertical (y) color gradient.
}
  \item{oticks}{
the margin where to put the colorbar ticks relative to the colorbar rectangle (\code{'l'} left, \code{'r'} right and \code{'b'} for bottom;
}
  \item{cb.title}{
character string indicating the title of the colorbar (default is set to date information/empty string if date information is missing.)
}
  \item{cb.xlab}{
character string indicating the x-axis label of the colorbar.
}
}
\details{
\code{set.colobar} adds a colorbar to the current plot device. If colorbar positions are missing (\code{cbx}, \code{cby}), the user will be asked for manual placement. \code{ticks} and tick-\code{labels} should correspond to \code{zlim}-values of the plot. \code{pal} defines the colormap and should equal \code{col} of the selected plot.
}
\value{
a list of colorbar definition vectors: \code{oticks, gradient, cbx} and \code{cby}. See function argmuments for more details.
}

\author{
Robert K. Bauer
}

\keyword{image plot}


\examples{
## Example 1: matrix 
vals <- rnorm(1000,500,100)
vals <- vals[order(vals)]
hist(vals)
z <- matrix(vals,100,100)
zlim <- c(0,1000)
par(mar=rep(5,4)) # add space for colorbar
data(cmap)
image(z,col=cmap$jet,zlim=zlim)
ticks <- seq(zlim[1],zlim[2],100)

set.colorbar(cbx=c(0,1),cby=c(-.16, -.248), ticks=ticks) # bottom
set.colorbar(cby=c(0,1),cbx=c(-.16, -.088), ticks=ticks) # left
set.colorbar(cbx=c(0,1),cby=c(1.088, 1.16), ticks=ticks) # top
set.colorbar(cby=c(0,1),cbx=c(1.088, 1.16), ticks=ticks) # right 

## manual placement calling the widget
# cb <- set.colorbar(ticks=ticks)

## reuse definition
# image(z,col=cmap$jet,zlim=zlim)
# set.colorbar(cbx=cb$cbx, cby=cb$cby, ticks=ticks)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{widget}
\keyword{colorbar}% __ONLY ONE__ keyword per line
