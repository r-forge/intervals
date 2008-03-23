\name{size}

\alias{size}
\alias{size,Intervals-method}
\alias{size,Intervals_full-method}

\title{Compute interval sizes}

\description{
  Compute the size, in either Z or R as appropriate, for each interval
  in an interval matrix.
}

\usage{
size(x)
}

\arguments{
  \item{x}{An \code{"Intervals"} or \code{"Intervals_full"} object.}
}

\details{
  For type \code{"Z"} objects, counting measure; for type \code{"R"}
  objects, Lebesgue measure.
}

\value{
  A numeric vector with length equal to \code{nrow(x)}.
}

\seealso{
  See \code{\link{empty}} to identify empty intervals. Note that when
  \code{type(x) == "R"}, a size of 0 does not imply that an interval is
  empty.
}

\examples{
z1 <- new( "Intervals", cbind( 1, 1:3 ), type = "Z" )
z2 <- z1; closed(z2)[1] <- FALSE
z3 <- z1; closed(z3) <- FALSE

size(z1)
size(z2)
size(z3)

r1 <- z1; type(r1) <- "R"
r2 <- z2; type(r2) <- "R"
r3 <- z3; type(r3) <- "R"

size(r1)
size(r2)
size(r3)

s1 <- new( "Intervals_full", matrix( 1, 3, 2 ), type = "Z" )
closed(s1)[2,2] <- FALSE
closed(s1)[3,] <- FALSE

size(s1)
}

\keyword{}
