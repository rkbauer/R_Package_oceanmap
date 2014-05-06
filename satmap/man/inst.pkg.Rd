\name{inst.pkg}
\alias{inst.pkg}
\title{
Loading Packages and automatically installs packages if missing}
\description{
Loads and automatically installs packages if missing}
\usage{
inst.pkg(package)
}
\arguments{
  \item{package}{
the name of a package to be loaded, given as a name or character string. 
}
}
\details{
\code{inst.pkg} is based on \link{install.packages} and \link{library}.
}

\author{
Robert K. Bauer
}

\examples{
inst.pkg(satmap) # loading existing package
inst.pkg(marmap) # install and load new package
}