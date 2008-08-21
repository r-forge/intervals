\name{distance_to_nearest}

\alias{distance_to_nearest}
\alias{distance_to_nearest,numeric,Intervals_virtual-method}
\alias{distance_to_nearest,Intervals_virtual,Intervals_virtual-method}

\title{Compute distance to nearest position in a set of intervals}

\description{
  For each point or interval in the \code{from} argument, compute the
  distance to the nearest position in the \code{to} argument.
}

\usage{
\S4method{distance_to_nearest}{numeric,Intervals_virtual}(from, to)

\S4method{distance_to_nearest}{Intervals_virtual,Intervals_virtual}(from, to)
}

\arguments{
  
  \item{from}{An object of appropriate type.}

  \item{to}{An object of appropriate type.}
  
}

\value{
  A vector of distances, with one entry per point or interval in \code{from}.
}

\note{
  Distances are computed efficiently using \code{\link{approxfun}}.
}

\examples{
# Point to iterval

to <- new( "Intervals", c(0,5,3,Inf) )
from <- -5:10
plot( from, distance_to_nearest( from, to ), type = "l" ) 
segments( to[,1], 1, pmin(to[,2], par("usr")[2]), 1, col = "red" )

# Interval to interval

from <- new( "Intervals", c(-Inf,-Inf,3.5,-1,1,4) )
distance_to_nearest( from, to )
}
