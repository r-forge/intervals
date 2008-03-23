\name{close_intervals}

\alias{close_intervals}
\alias{close_intervals,Intervals-method}
\alias{close_intervals,Intervals_full-method}

\alias{open_intervals}
\alias{open_intervals,Intervals-method}
\alias{open_intervals,Intervals_full-method}

\title{Represent integer intervals with open or closed endpoints}

\description{
  Given an integer interval matrix, adjust endpoints so that all
  intervals are closed at both ends.
}

\usage{
close_intervals(x)
open_intervals(x)
}

\arguments{
  \item{x}{
    An object of appropriate class, and for which \code{x@type ==
    "Z"}. If \code{x@type == "R"}, an error is generated.
  }
}

\value{
  An object of the same class as \code{x}, with endpoints adjusted as
  necessary and all \code{closed(x)} set to either \code{TRUE} or
  \code{FALSE}, as appropriate.
}

\section{Methods}{
  \describe{
    \item{x = \code{"Intervals"}:}{See above.}
    \item{x = \code{"Intervals_full"}:}{See above.}
  }
}

\keyword{}
