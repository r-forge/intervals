\name{Intervals-class}

\docType{class}

\alias{Intervals-class}
\alias{Intervals_full-class}

\title{Classes "Intervals" and "Intervals_full"}

\description{
  \code{"Intervals"} objects are two-column matrices which represent sets,
  possibly non-disjoint, of intervals on either the integers or the real
  line. All intervals in each object have the same endpoint closure
  pattern. \code{"Intervals_full"} objects are similar, but permit
  interval-by-interval endpoint closure specification.
}

\section{Slots}{
  \describe{
    
    \item{\code{.Data}:}{
      Object of class \code{"matrix"}. A two-column, numeric format is
      required. Only integral (though not \code{"integer"} class)
      endpoints are permitted if \code{type} is \code{"Z"}.
    }
    
    \item{\code{closed}:}{
      For \code{"Intervals"} objects, a two-element logical vector. For
      \code{"Intervals_full"} objects, a two-column logical matrix with
      the same dimensions as \code{.Data}. 
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

\note{No notes at the moment.}

\examples{
# The "Intervals" class

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

# Full endpoint control

j <- i
closed(j)[ 3:4, 2 ] <- FALSE

print(j)
}

\keyword{classes}
