\name{interval_complement}

\alias{interval_complement}
\alias{interval_complement,Intervals-method}
\alias{interval_complement,Intervals_full-method}

\title{Compute the complement of a set of intervals}

\description{
  Compute the complement of a set of intervals.
}

\usage{
interval_complement(x)
}

\arguments{  
  \item{x}{An \code{Intervals} or \code{Intervals_full} object.}
}

\value{
  An object of the same class as \code{x}, compactly representing the
  complement of the intervals described in \code{x}.
}

\note{
  For objects of class \code{Intervals}, closure on \code{-Inf} or
  \code{Inf} endpoints is set to match that of all the intervals with
  finite endpoints. For objects of class \code{Intervals_full},
  non-finite endpoints are always open.
}

