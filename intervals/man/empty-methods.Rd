\name{empty}

\alias{empty}
\alias{empty,Intervals-method}
\alias{empty,Intervals_full-method}

\title{Identify empty interval rows}

\description{
  A valid interval matrix may contain empty intervals: those with common
  endpoints, at least one of which is open. The \code{empty} method
  identifies these rows.
}

\usage{
\S4method{empty}{Intervals}(x, tolerance = .Machine$double.eps^0.5)

\S4method{empty}{Intervals_full}(x, tolerance = .Machine$double.eps^0.5)
}

\arguments{
  
  \item{x}{An \code{"Intervals"} or \code{"Intervals_full"} object.}

    \item{tolerance}{
    Assessing endpoint equality with floating point endpoints requires
    an approximation. In this function, we compare relative difference
    (difference divided by maximum absolute value) to \code{tolerance}
    to decide if two endpoints are in fact at the same position. The
    default tolerance here is the same as is used in
    \code{\link{all.equal.numeric}}, although
    \code{\link{all.equal.numeric}} uses an \emph{asymmetic} relative
    difference, with \code{target} in the denominator, not a maximum.
  }
  
}

\details{
  Intervals are deemed to be empty when their endpoints are equal (up to
  a relative difference of \code{tolerance}) and not both closed. The
  matrices \code{x} and \code{x[!empty(x),]} represent the same subset
  of the integers or the real line.
}

\value{
  A boolean vector with length equal to \code{nrow(x)}.
}

\note{
  Note that intervals of size 0 may not be empty over the reals, and
  intervals whose second endpoint is strictly greater than the first
  \emph{may} be empty over the integers, if both endpoints are open.
}

\seealso{
  See \code{\link{size}} to compute the size of each interval in an
  object.
}

\examples{
z1 <- new( "Intervals", cbind( 1, 1:3 ), type = "Z" )
z2 <- z1; closed(z2)[1] <- FALSE
z3 <- z1; closed(z3) <- FALSE

empty(z1)
empty(z2)
empty(z3)

r1 <- z1; type(r1) <- "R"
r2 <- z2; type(r2) <- "R"
r3 <- z3; type(r3) <- "R"

empty(r1)
empty(r2)
empty(r3)

s1 <- new( "Intervals_full", matrix( 1, 3, 2 ), type = "Z" )
closed(s1)[2,2] <- FALSE
closed(s1)[3,] <- FALSE

empty(s1)

# Floating point and tolerance

x <- new("Intervals", c( .99, 1 ), closed = FALSE)
empty(x)
empty(x, tolerance = .02)
closed(x) <- TRUE
empty(x, tolerance = .02)
}

