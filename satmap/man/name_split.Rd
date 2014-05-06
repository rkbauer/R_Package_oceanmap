\name{name_split}
\alias{name_split}
\title{
Returns a summary table of an \code{'.gz'}-filen based on its name
}

\description{
Returns a summary table of an \code{'.gz'}-filen based on its name
}

\value{
Returns a summary table of a \code{'.gz'}-file (by splitting its name) with the header:

\code{area source parameter resolution timestep date1 date2 option}

\item{\code{area}}{
region keyword
}

\item{\code{source}}{
data source
}

\item{\code{param}}{
the parameter saved in the \code{'.gz'}-file. Can only be one value!
}

\item{\code{resolution}}{
the spatial resolution
}

\item{\code{timestep}}{
the temporal resolution
}

\item{\code{date1} & \code{date2}}{
the temporal resolution
the time interval covered
}


\item{\code{option}}{
a character string holding supplmentary information of \code{'.gz'}-file treatment
}
}

\usage{
name_split(filename)
}
\arguments{
  \item{filename}{
Character string indicating search criteria for sat files. 
}
}
\seealso{See \link{check_gzfiles} to return summary of available \code{'.gz'}-files and \link{name_join} to create \code{'.gz'}-filenames from splitted names (\link{name_split})-calls} 


\author{
Herve Demarq, translated from IDL by Robert K. Bauer
}

\examples{
## Example: read and plot '.gz'-file
owd <- getwd()
setwd(system.file("test_files", package="satmap"))
check_gzfiles() # return file summary-table per filetype
gz.files <- Sys.glob('*.gz') # load sample-'.gz'-files

# return summary of availble '.gz'-files
# suffix-column corresponds to option column of the name_split-call
# addition n-column returns the number of available files per filetype
check_gzfiles(gz.files)

## Example: split and rejoin '.gz'-filenames
gz.files
name_split(gz.files)# return summary-table per file
name_join(name_split(gz.files))
}
