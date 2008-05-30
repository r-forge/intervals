\name{combine}

\alias{combine}
\alias{combine,Intervals-method}
\alias{combine,Intervals_full-method}
\alias{combine,missing-method}

\title{Combine different interval matrix objects}

\description{
  Stack objects just like \code{\link{rbind}}. Since S4 methods cannot
  easily be written for \code{\link{rbind}} due to the lack of named
  arguments, we following the Biobase convention instead.
}

\usage{
combine(x, y, ...)
}

\arguments{
  
  \item{x}{An \code{"Intervals"} or \code{"Intervals_full"} object.}

  \item{y}{An \code{"Intervals"} or \code{"Intervals_full"} object.}

  \item{...}{More of the same.}
  
}

\details{
  All objects are expected to have the same class, and to have the same
  value in the \code{type} slot. A \code{NULL} in any argument is
  ignored.
}

\value{
  If no arguments are supplied, \code{combine} returns \code{NULL},
  similar to \code{\link{rbind}}. In all other cases, objects are
  stacked in their order of appearance in the the argument list. 
}

\note{
  The \code{"missing"} method permits convenient use of
  \code{\link{do.call}} with named lists. In such cases,
  \code{\link{do.call}} will only put list elements into the \code{x} or
  \code{y} arguments if they are explicitly named as such; otherwise,
  everything ends up in \code{...} and \code{x} and \code{y} are left
  empty. The \code{"missing"} method just strips names from the elements
  of \code{...} and then reapplies the method. \emph{Warning!} Applying
  \code{\link{do.call}} to a named list which coincidentally has
  named elements \code{"x"} or \code{"y"} may cause an unexpected
  reordering before combining.
}

