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
    \item{\code{\link{Intervals-class}}}{Classes \code{"Intervals"} and \code{"Intervals_full"}}
    \item{\code{\link{Intervals_virtual-class}}}{Class \code{"Intervals_virtual"}}
    \item{\code{\link{close_intervals}}}{Re-represent integer intervals with open or closed endpoints}
    \item{\code{\link{combine}}}{Combine different interval matrix objects}
    \item{\code{\link{clusters}}}{Identify clusters in a collection of positions or intervals}
    \item{\code{\link{distance_to_nearest}}}{Compute distance to nearest position in a set of intervals}
    \item{\code{\link{empty}}}{Identify empty interval rows}
    \item{\code{\link{expand}}}{Expand or contract sets}
    \item{\code{\link{interval_complement}}}{Compute the complement of a set of intervals}
    \item{\code{\link{interval_intersection}}}{Compute the intersection of one or more sets of intervals}
    \item{\code{\link{interval_overlap}}}{Assess which query intervals overlap which targets}
    \item{\code{\link{interval_union}}}{Compute the union of intervals in one or more interval matrices}
    \item{\code{\link{reduce}}}{Compactly re-represent the points in a set of intervals}
    \item{\code{\link{sgd}}}{Yeast gene model information}
    \item{\code{\link{size}}}{Compute interval sizes}
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
  See
  \code{\link[genomeIntervals:genomeIntervals-package]{genomeIntervals}},
  which extends the functionality of this package.
}
