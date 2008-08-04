\name{interval_intersection}

\alias{interval_intersection}
\alias{interval_intersection,Intervals_virtual-method}
\alias{interval_intersection,missing-method}

\title{Compute the intersection of one or more sets of intervals}

\description{
  Given one or more sets of intervals, produce a new set compactly
  representing points contained in at least one interval of each input
  object.
}

\usage{
interval_intersection(x, ...)
}

\arguments{
  \item{x}{ An \code{"Intervals"} or \code{"Intervals_full"} object.}
  \item{\dots}{Optionally, additional objects of the same class as \code{x}.}
}

\value{
  A single object of the same class as the input objects.
}
