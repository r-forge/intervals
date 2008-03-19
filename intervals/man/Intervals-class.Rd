\name{Intervals-class}

\docType{class}

\alias{Intervals-class}

\alias{[,Intervals-method}
\alias{coerce,Intervals,Intervals_full-method}
\alias{coerce,Intervals_full,Intervals-method}
\alias{rbind,Intervals-method}

\title{Class "Intervals"}

\description{
  "Intervals" objects are two-column matrices which represent sets,
  possibly non-disjoint, of intervals on either the integers or the real
  line. All intervals in each object have the same endpoint closure
  pattern.
}

% \section{Objects from the Class}{
%   Objects can be created by calls of the form \code{new("Intervals", ...)}.
% }

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{
      Object of class \code{"matrix"}. A two-column, numeric format is
      required. Only integral (though not \code{"integer"} class)
      endpoints are permitted if \code{type} is \code{"Z"}.
    }
    \item{\code{closed}:}{
      Object of class \code{"logical"}. A two-element vector,
      corresponding to each of the two endpoint columns, is required.
    }
    \item{\code{type}:}{
      Object of class \code{"character"}. A one-element character vector
      with either \code{"Z"} or \code{"R"} is required.
    }
  }
}

\section{Extends}{
  
  Class \code{"\linkS4class{Intervals_virtual}"}, directly.

  Class \code{"\linkS4class{matrix}"}, by class "Intervals_virtual", distance 2.

  Class \code{"\linkS4class{array}"}, by class "Intervals_virtual", distance 3.

  Class \code{"\linkS4class{structure}"}, by class "Intervals_virtual", distance 4.

  Class \code{"\linkS4class{vector}"}, by class "Intervals_virtual", distance 5, with explicit coerce.
  
}

\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Intervals")}: ... }
    \item{coerce}{\code{signature(from = "Intervals", to = "Intervals_full")}: ... }
    \item{coerce}{\code{signature(from = "Intervals_full", to = "Intervals")}: ... }
    \item{rbind}{\code{signature(x = "Intervals")}: ... }
	 }
}

\author{Richard Bourgon}

\note{No notes at the moment.}

\seealso{
  See \code{"\linkS4class{Intervals_full}"} for interval-level endpoint
  closure specification.
}

\examples{
i <- new(
	 "Intervals",
	 matrix(
	 	c(1,2,  
	 	  3,5,
	 	  4,6,
	 	  8,9
	 	  ),
	 	byrow = TRUE,
                ncol = 2
	 ),
	 closed = c( TRUE, TRUE ),
	 type = "Z"
	 )
colnames(i) <- c( "first", "last" )

# Row subsetting preserves class. Column subsetting causes coercion to
# "matrix" class. 

print(i)
print(i[1:2,])
print(i[,1:2])
}

\keyword{classes}
