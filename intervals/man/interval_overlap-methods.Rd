\name{interval_overlap}

\alias{interval_overlap}
\alias{interval_overlap,Intervals_virtual,Intervals_virtual-method}

\title{Assess which query intervals overlap which targets}

\description{
  Given two objects, a query and a target, assess which query intervals
  overlap which targets.
}

\usage{
\S4method{interval_overlap}{Intervals_virtual,Intervals_virtual}(query, target, check_valid = TRUE)
}

\arguments{
  
  \item{query}{An \code{"Intervals"} or \code{"Intervals_full"} object.}

  \item{target}{
    An \code{"Intervals"} or \code{"Intervals_full"} object. The
    \code{target} object must have the same \code{\link{type}} as
    \code{query}. 
  }
  
  \item{check\_valid}{
    Should \code{\link{validObject}} be called before passing to
    compiled code? This, among other things, verifies that endpoints are
    of data type \code{numeric} and the \code{closed} vector/matrix is
    appropriately sized and of the correct data type. (Compiled code
    does no further checking.)
  }
  
}

\details{
  Intervals which meet at endpoints overlap only if both endpoints are
  closed. Intervals in \code{query} with \code{NA} endpoints are
  ignored, with a warning; in \code{target}, such interval produce no
  matches. Intervals in either \code{query} or \code{target} which are
  actually empty have their endpoints set to \code{NA} before
  proceeding, with warning, and so do not generate matches.
}

\value{
  A list, with one element for each row of \code{target}. The elements
  are vectors of indices, indicating which \code{query} rows overlap
  each target. A list element of length 0 indicates a target with no
  overlapping query intervals.
}

\note{
  If you want real (\code{type = "R"}) intervals that overlap in a set
  of positive measure -- not just at endpoints -- set all endpoints to
  open (\code{closed(query) <- FALSE}). 
}

\examples{
# Note that t contains a valid but empty interval.

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
rownames(t) <- letters[1:nrow(t)]

empty(q)
empty(t)

interval_overlap(q, t)

# Non-empty real intevals of size 0 can overlap other intervals.

u <- q
type(u) <- "R"

v <- new("Intervals_full", rep(3,4))
closed(v)[2,] <- FALSE
v
empty(v)
size(v)

interval_overlap( u, v )
}
