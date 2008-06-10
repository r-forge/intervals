\name{interval_union}

\alias{interval_union}
\alias{interval_union,Intervals_virtual-method}
\alias{interval_union,missing-method}

\title{Compute the union of intervals in one or more interval matrices}

\description{
  Compute the union of intervals in one or more interval matrices. The
  intervals contained in a single interval matrix object need not, in
  general, be disjoint; \code{interval_union}, however, always returns a
  matrix with sorted, disjoint intervals.
}

\usage{
interval_union(x, ..., tolerance = .Machine$double.eps^0.5, check_valid = TRUE)
}

\arguments{

  \item{x}{ An \code{"Intervals"} or \code{"Intervals_full"} object.}
  
  \item{\dots}{Optionally, additional objects of the same class as \code{x}.}
  
  \item{tolerance}{
    Assessing overlap with floating point endpoints requires an
    approximate definition of equality. See comments for the
    \code{tolerance} argument of \code{\link{interval_overlap}}. 
  }
  
  \item{check_valid}{
    Should \code{\link{validObject}} be called before passing to
    compiled code? Also see \code{\link{interval_overlap}}.
  }
  
}

\value{
  A single object of appropriate class, compactly representing the union
  of all intervals in \code{x}, and optionally, in \code{...} as
  well. For class \code{"Intervals"}, the result will have the same
  \code{closed} values as \code{x}.
}
