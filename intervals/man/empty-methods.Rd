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
empty(x)
}

\arguments{
  \item{x}{An \code{"Intervals"} or \code{"Intervals_full"} object.}
}

\details{
  E.g., take [\emph{a},\emph{b}) to represent points greater than or equal to
  \emph{a} but also less \emph{b}. Note that intervals of size 0 may not
  be empty over the reals, and intervals whose second endpoint is
  strictly greater than the first \emph{may} be empty over the integers.
}

\value{
  A boolean vector with length equal to \code{nrow(x)}. The matrices
  \code{x} and \code{x[!empty(x),]} represent the same subset of the
  integers or the real line.
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

s1 <- new( "Intervals_full", matrix( 1, ncol=2, nrow=3 ), type = "Z" )
closed(s1)[2,2] <- FALSE
closed(s1)[3,] <- FALSE

empty(s1)
}

\keyword{}
