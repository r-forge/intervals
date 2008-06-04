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
    approximate definition of equality. In this function, we compare
    relative difference (difference divided by maximum absolute value)
    to \code{tolerance} to decide if two endpoints are in fact at the
    same position. The default tolerance here is the same as is used in
    \code{\link{all.equal.numeric}}, although
    \code{\link{all.equal.numeric}} uses an \emph{asymmetic} relative
    difference, with \code{target} in the denominator, not a maximum.
  }

  \item{check_valid}{
    Should \code{\link{validObject}} be called before passing to
    compiled code? This, among other things, verifies that endpoints are
    of data type \code{numeric} and the \code{closed} vector/matrix is
    appropriately sized and of the correct data type. (Compiled code
    does no further checking.)
  }
  
}

\details{
  Intervals which meet at endpoints (up to a relative difference of
  \code{tolerance}) overlap only if both endpoints are closed. Intervals
  in \code{query} with \code{NA} endpoints are ignored, with a warning; in
  \code{target}, such interval produce no matches.
}

\value{
  A list, with one element for each row of \code{target}. The elements
  are vectors of indices, indicating which \code{query} rows overlap
  each target. A list element of length 0 indicates a target with no
  overlapping query intervals.
}

\examples{
q <- new(
         "Intervals",
         matrix(
                c(
                  2,  8,
                  3,  4,
                  5, 10
                  ),
                ncol = 2, byrow = TRUE
                ),
         closed = c( TRUE, FALSE ),
         type = "Z"
         )

t <- new(
         "Intervals",
         matrix(
                c(
                   2,  8,
                   8,  9,
                   6,  9,
                  11, 12,
                   3,  3
                  ),
                ncol = 2, byrow = TRUE
                ),
         closed = c( TRUE, FALSE ),
         type = "Z"
         )

names(t) <- letters[1:nrow(t)]

interval_overlap(q, t)
}

