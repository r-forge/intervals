\name{intervals-package}

\alias{intervals-package}
\alias{intervals}

\docType{package}

\title{
Tools for working with points and intervals
}

\description{
Tools for working with and comparing sets of points and intervals.
}

\details{
  
  Index:
  \describe{
    \item{\code{\link{Intervals-class}}}{Classes \code{"Intervals"} and \code{"Intervals_full"}.}
    \item{\code{\link{Intervals_virtual-class}}}{Class \code{"Intervals_virtual"}.}
    \item{\code{\link[=c.Intervals]{c}}}{Concatenate different sets of intervals.}
    \item{\code{\link{close_intervals}}}{Re-represent integer intervals with open or closed endpoints.}
    \item{\code{\link{closed}}}{Accessor for \code{closed} slot: closure vector/matrix.}
    \item{\code{\link{clusters}}}{Identify clusters in a collection of positions or intervals.}
    \item{\code{\link{contract}}}{Contract sets.}
    \item{\code{\link{distance_to_nearest}}}{Compute distance to nearest position in a set of intervals.}
    \item{\code{\link{empty}}}{Identify empty interval rows.}
    \item{\code{\link{expand}}}{Expand sets.}
    \item{\code{\link{interval_complement}}}{Compute the complement of a set of intervals.}
    \item{\code{\link{interval_difference}}}{Compute set difference.}
    \item{\code{\link{interval_intersection}}}{Compute the intersection of one or more sets of intervals.}
    \item{\code{\link{interval_overlap}}}{Assess which query intervals overlap which targets.}
    \item{\code{\link{interval_union}}}{Compute the union of intervals in one or more interval matrices.}
    \item{\code{\link{is.na}}}{Identify interval rows with \code{NA} endpoints.}
    \item{\code{\link{reduce}}}{Compactly re-represent the points in a set of intervals.}
    \item{\code{\link{sgd}}}{Yeast gene model sample data.}
    \item{\code{\link{size}}}{Compute interval sizes.}
    \item{\code{\link{type}}}{Accessor for \code{type} slot: Z or R.}
  }
  
  Further information is available in the following vignettes:
  \describe{
    \item{\code{intervals_overview}}{Overview of the intervals package.}    
  }

}

\author{
Richard Bourgon <bourgon@ebi.ac.uk>
}

\keyword{ package }

\section{Acknowledgments}{
  Thanks to Julien Gagneur, Simon Anders, and Wolfgang Huber for
  numerous helpful suggestions about the package content and code.
}

\seealso{
  See the \code{genomeIntervals} package in Bioconductor, which extends
  the functionality of this package.
}
