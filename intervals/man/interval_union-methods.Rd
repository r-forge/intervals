\name{interval_union}

\alias{interval_union}
\alias{interval_union,Intervals-method}
\alias{interval_union,Intervals_full-method}
\alias{interval_union,missing-method}

\title{Compute the union of intervals in one or more interval matrices}

\description{
  Compute the union of intervals in one or more interval matrices. The
  intervals contained in a single interval matrix object need not, in
  general, be disjoint; \code{interval_union}, however, always returns a
  matrix with sorted, disjoint intervals.
}

\usage{
interval_union(x, ...)
}

\arguments{
  \item{x}{ An \code{"Intervals"} or \code{"Intervals_full"} object.}
  \item{\dots}{Optionally, additional objects of the same class as \code{x}.}
}

\value{
  A single object of appropriate class, compactly representing the union
  of all intervals in \code{x}, and optionally, in \code{...} as
  well. For class \code{"Intervals"}, the result will have the same
  \code{closed} values as \code{x}.
}
