\name{which_nearest}

\alias{which_nearest}
\alias{which_nearest,numeric,Intervals_virtual-method}
\alias{which_nearest,Intervals_virtual,Intervals_virtual-method}

\title{Identify nearest member(s) in a set of intervals}

\description{
  For each point or interval in the \code{from} argument,
  identify the nearest member or members (in case of ties) of the
  interval set in the \code{to} argument.
}

\usage{
\S4method{which_nearest}{numeric,Intervals_virtual}(from, to, check_valid = TRUE)

\S4method{which_nearest}{Intervals_virtual,Intervals_virtual}(from, to, check_valid = TRUE)
}

\arguments{
  
  \item{from}{An object of appropriate type.}

  \item{to}{An object of appropriate type.}
  
  \item{check_valid}{
    Should \code{\link{validObject}} be called before passing to
    compiled code? Also see \code{\link{interval_overlap}}.
  }

}

\value{
  A list, with one element per interval in the \code{from} argument,
  giving the row index or indices (in case of ties) of the nearest
  member (or members) of \code{to}. \code{NA} entries in \code{from}
  produce an empty vector of indices, as do cases when \code{to}
  contains no non-empty intervals.

  CHECK NA BEHAVIOR!
  
  % A vector of distances, with one entry per point or interval in
  % \code{from}. Any intervals in \code{from} which are either empty (see
  % \code{\link{empty}}) or have \code{NA} endpoints produce a \code{NA}
  % result. 
}

\note{
  Code simply expands each interval in \code{from} by the distance to
  its nearest neighbor or neighbors, and then uses
  \code{\link{interval_overlap}} to identify the relevant intervals in
  \code{to}. 
}

\examples{
# Point to interval. Empty rows, or those with NA endpoints, do not
# generate hits.

to <- Intervals_full( c(-1,0,NA,5, -1,3,10,Inf) )
closed(to)[1,] <- FALSE
from <- c( NA, -3:5 )

to

data.frame( from = from, nearest = I( which_nearest( from, to ) ) )

which_nearest( from, to[1,] )

# Interval to interval

from <- Intervals( c(-Inf,-Inf,3.5,-1,1,4) )
which_nearest( from, to )
}
