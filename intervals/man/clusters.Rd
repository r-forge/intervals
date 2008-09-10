\name{clusters}
\
alias{clusters}

\title{Identify clusters in a vector of positions}

\description{
  This function uses tools in the \pkg{intervals} package to quickly
  identify clusters -- collections of positions each of which is no more
  than a given distance from at least one other member of the cluster --
  in a vector of positions.
}

\usage{
clusters(pos, w, which = FALSE)
}

\arguments{
  \item{pos}{A numeric vector.}
  \item{w}{
    Maximum permitted distance between a cluster member and at least one
    other member.
  }
  \item{which}{
    Should indices into the \code{pos} vector be returned instead of
    actual positions? Default is to return actual positions, i.e.,
    values from the \code{pos} vector.
  }
}

\details{
  A cluster is defined to be a set of two or more components of
  \code{pos}, each of which has at least one other cluster member within
  distance \code{w}.
}

\note{
  Implementation is by one call to \code{\link{reduce}} and one call to
  \code{\link{interval_overlap}}, and the function, while useful, is
  included to illustrate the utility of the core functions in the
  \pkg{intervals} package.
}

\value{
  A list whose components are the clusters. Each component is thus a
  vector of positions, or, if \code{which = TRUE}, a vector of
  indices into the \code{pos} vector.
}
