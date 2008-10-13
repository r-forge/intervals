\name{close_intervals}

\alias{close_intervals}
\alias{close_intervals,Intervals_virtual-method}

\alias{open_intervals}
\alias{open_intervals,Intervals_virtual-method}

\alias{adjust_closure}
\alias{adjust_closure,Intervals-method}
\alias{adjust_closure,Intervals_full-method}


\title{Re-represent integer intervals with open or closed endpoints}

\description{
  Given an integer interval matrix, adjust endpoints so that all
  intervals have the requested closure status.
}

\usage{
\S4method{close_intervals}{Intervals_virtual}(x)

\S4method{open_intervals}{Intervals_virtual}(x)

\S4method{adjust_closure}{Intervals}(x, close_left = TRUE, close_right = TRUE)

\S4method{adjust_closure}{Intervals_full}(x, close_left = TRUE, close_right = TRUE)
}

\arguments{
  
  \item{x}{
    An object of appropriate class, and for which \code{x@type ==
    "Z"}. If \code{x@type == "R"}, an error is generated.
  }
  
  \item{close\_left}{
    Should the left endpoints be closed or open?
  }

  \item{close\_right}{
    Should the right endpoints be closed or open?
  }
  
}

\value{
  An object of the same class as \code{x}, with endpoints adjusted as
  necessary and all \code{closed(x)} set to either \code{TRUE} or
  \code{FALSE}, as appropriate.
}

\note{
  The \code{close_intervals} and \code{open_intervals} are for
  convenience, and just call \code{adjust_closure} with the approriate
  arguments.
  
  The \code{x} object may contain empty intervals, with at least one
  open endpoint, and still be valid. (Intervals are invalid if their
  second endpoint is less than their first.) The \code{close_intervals}
  method would, in such cases, create an invalid result; to prevent
  this, empty intervals are detected and removed, with a warning.
}

\examples{
x <- Intervals(
               c( 1, 5, 10, 1, 6, 20 ),
               closed = c( TRUE, FALSE ),
               type = "Z"
               )

# Empties are dropped
close_intervals(x)
adjust_closure(x, FALSE, TRUE)

# Intervals_full
y <- as( x, "Intervals_full" )
closed(y)[1,2] <- TRUE
open_intervals(y)
}