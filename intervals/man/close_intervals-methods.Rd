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
  intervals are closed at the requested ends.
}

\usage{
close_intervals(x, left = TRUE, right = TRUE)
open_intervals(x, left = TRUE, right = TRUE)
}

\arguments{
  
  \item{x}{
    An object of appropriate class, and for which \code{x@type ==
    "Z"}. If \code{x@type == "R"}, an error is generated.
  }
  
  \item{left}{
    Should the left endpoints be closed?
  }

  \item{right}{
    Should the right endpoints be closed?
  }
  
}

\value{
  An object of the same class as \code{x}, with endpoints adjusted as
  necessary and all \code{closed(x)} set to either \code{TRUE} or
  \code{FALSE}, as appropriate.
}

\note{
  The \code{x} object may contain empty intervals, with at least one
  open endpoint, and still be valid. (Intervals are invalid if their
  second endpoint is less than their first.) The \code{close_intervals}
  method would, in such cases, create an invalid result; to prevent
  this, empty intervals are detected and removed, with a warning.
}

