\name{rd_equal}

\alias{rd_equal}

\title{Test approximate equality using relative difference}

\description{
  Algorithms in this package need to be able to accurately determine
  when interval endpoints coincide. If floating point values are used
  for endpoints, required exact equality is not appropriate, so relative
  difference is used instead.
}

\usage{
rd_equal(x, y, tolerance = .Machine$double.eps^0.5)
}

\arguments{
  \item{x}{A vector of values to be compared, point-wise, to \code{y}.}
  \item{y}{A vector fo values of the same length as \code{x}.}
  \item{tolerance}{
    Tolerance for establishing approximate equality. The default value
    is the same one used by \code{\link{all.equal.numeric}},
  }
}

\details{
  For each component in \code{x} and its partner in \code{y}, the
  absolute value of difference is compared to the larger of the two
  individual absolute values. The values are deemed to be equal if the
  ratio is less than \code{tolerance}. When one or both values are
  infinite, a simple comparison with \code{==} is made; when one or both
  are \code{NA}, a \code{NA} is returned.
}

\value{
  A logical vector of the same length as \code{x} and \code{y}.
}

\seealso{
  See \code{\link{all.equal.numeric}} for related, though distinct,
  functionality. Specifically, the comparisons made by
  \code{\link{all.equal.numeric}} are relative to the \emph{mean}
  absolute value of the target; \code{rd_equal}, on the other hand,
  makes pointwise relative difference comparisons.
}

\examples{
eps <- 1e-5
x <- 1:2
y <- x + c( eps, 0 )
rd_equal(x, y)

# The relative difference for all.equal() uses the average across the
# target vector in the denominator, while rd_equal() computes the
# denominator coordinate-wise:

rd_equal(x, y, tolerance = eps)
all.equal(x, y, tolerance = eps)

# Handling non-finite values

rd_equal(
         c( .8, .9, NA, Inf, -Inf ),
         c(  1,  1,  1, Inf,  Inf ),
         tol = .1
         )

}
