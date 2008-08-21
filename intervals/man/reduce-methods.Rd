\name{reduce}

\alias{reduce}
\alias{reduce,Intervals_virtual-method}

\title{Compactly re-represent the points in a set of intervals}

\description{
  In general, \code{\linkS4class{Intervals}} and
  \code{\linkS4class{Intervals_full}} objects may be redundant, and the
  intervals they contain may be in arbitrary order. The \code{reduce}
  function re-represents the underlying subsets of the integers or the
  real line in the unique, minimal form.
}

\usage{
\S4method{reduce}{Intervals_virtual}(x, tolerance = .Machine$double.eps^0.5, check_valid = TRUE)
}

\arguments{

  \item{x}{ An \code{"Intervals"} or \code{"Intervals_full"} object.}
  
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
  A single object of appropriate class, compactly representing the
  union of all intervals in \code{x}. For class \code{"Intervals"}, the
  result will have the same \code{closed} values as \code{x}.
}

\seealso{
 See \code{\link{interval_union}}, which is really just a wrapper for
 \code{reduce}. 
}