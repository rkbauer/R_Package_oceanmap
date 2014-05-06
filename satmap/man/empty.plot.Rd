\name{empty.plot}
\alias{empty.plot}
\title{
Creates an empty scatter plot}
\description{
Creates an empty scatter plot that is equal to the function call:

\code{plot(1,lwd=0,axes=F,xlab="",ylab="",...)}
}
\usage{
empty.plot(..., axes = F, xlab = "", ylab = "")
}
\arguments{

 \item{axes}{
whether to show plot axes (default is \code{FALSE}).
}

 \item{xlab, ylab}{
label for the x- and y-axis of the plot (default is empty).
}

 \item{...}{
Other arguments to be passed either to \code{plot}.
}
}

\author{
Robert K. Bauer
}


\examples{
empty.plot()
title("empty plot")
box()
axis(1)
axis(2)
}