\name{interval_overlap}

\alias{interval_overlap}
\alias{interval_overlap,Intervals_virtual,Intervals_virtual-method}

\title{Assess which query intervals overlap which targets}

\description{
  Given two objects, a query and a target, assess which query intervals
  overlap which targets.
}

\usage{
interval_overlap(
                 query, target,
                 tolerance = .Machine$double.eps^0.5,
                 check_valid = TRUE
                 )
}

\arguments{
  
  \item{query}{An \code{"Intervals"} or \code{"Intervals_full"} object.}

  \item{target}{An \code{"Intervals"} or \code{"Intervals_full"} object.}

  \item{tolerance}{
    Assessing overlap with floating point endpoints requires an
    approximate definition of equality. Here, we compare relative
    difference (difference minus the maximum absolute value) to
    \code{tolerance}. The same default as used in
    \code{\link{all.equal.numeric}} is used here, although
    \code{\link{all.equal.numeric}} uses an \emph{asymmetic} relative
    difference, with \code{target} in the denominator, not a maximum.
  }

  \item{check_valid}{
    Should \code{\link{validObject}} be called before passing to
    compiled code? This, among other things, verifies that endpoints are
    of data type \code{numeric} and the \code{closed} vector/matrix is
    appropriately sized.
  }
  
}

\details{
}

\value{
}

\note{
}

