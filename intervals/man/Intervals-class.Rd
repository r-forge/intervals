\name{Intervals-class}

\docType{class}

\alias{Intervals-class}
\alias{Intervals_full-class}
\alias{Intervals_virtual-class}

\alias{closed}
\alias{closed,Intervals_virtual-method}
\alias{closed<-}
\alias{closed<-,Intervals-method}
\alias{closed<-,Intervals_full-method}

\alias{coerce,Intervals,Intervals_full-method}
\alias{coerce,Intervals_full,Intervals-method}
\alias{coerce,Intervals_virtual,character-method}

\alias{head,Intervals_virtual-method}

\alias{initialize,Intervals_full-method}

\alias{rbind}
\alias{rbind,Intervals-method}
\alias{rbind,Intervals_full-method}
\alias{rbind,Intervals_virtual-method}

\alias{show,Intervals_virtual-method}

\alias{t,Intervals_virtual-method}

\alias{tail,Intervals_virtual-method}

\alias{type}
\alias{type,Intervals_virtual-method}
\alias{type<-}
\alias{type<-,Intervals_virtual-method}

\alias{[,Intervals-method}
\alias{[,Intervals_full-method}




\title{Classes "Intervals" and "Intervals_full"}

\description{
  \code{Intervals} objects are two-column matrices which represent
  sets, possibly non-disjoint and in no particular order, of intervals
  on either the integers or the real line. All intervals in each object
  have the same endpoint closure pattern. \code{Intervals_full}
  objects are similar, but permit interval-by-interval endpoint closure
  specification.
}

\section{Slots}{
  \describe{
    
    \item{\code{.Data}:}{
      Object of class \code{matrix}. A two-column, numeric (see below)
      format is required. For a valid object, no value in the first
      column may exceed its partner in the second column. (Note that
      this \emph{does} permit empty interval rows, when both endpoints
      are of equal value and not both closed.) Only integral (though not
      \code{integer} class) endpoints are permitted if \code{type} is
      \code{"Z"}.
    }
    
    \item{\code{closed}:}{
      For \code{Intervals} objects, a two-element logical vector. For
      \code{Intervals_full} objects, a two-column logical matrix with
      the same dimensions as \code{.Data}. If omitted in a \code{new}
      call, the \code{closed} slot will be initialized to an object of
      appropriate type and size, with all entries \code{TRUE}. If
      \code{closed} is a vector of length 1, or a vector of length 2 for
      the \code{Intervals_full} class, an appropriate object will be
      made by reusing the supplied values row-wise. See the example
      below.
    }

    \item{\code{type}:}{
      Object of class \code{character}. A one-element character vector
      with either \code{"Z"} or \code{"R"} is required.
    }
    
  }
}

\section{Extends}{
  
  Class \code{Intervals_virtual}, directly.

  Class \code{\linkS4class{matrix}}, by class
  \code{Intervals_virtual}, distance 2.

  Class \code{\linkS4class{array}}, by class
  \code{Intervals_virtual}, distance 3.

  Class \code{\linkS4class{structure}}, by class
  \code{Intervals_virtual}, distance 4.

  Class \code{\linkS4class{vector}}, by class
  \code{Intervals_virtual}, distance 5, with explicit coerce.
  
}

\section{Methods}{
  \describe{
    
    \item{\code{[}:}{
      When used to subset rows, class is preserved; when used to subset
      columns, the \code{closed} and \code{type} slots are discarded and
      an appropriately subset version of \code{.Data} is returned. See
      example below.
    }
    
    \item{\code{closed}:}{ Accessor for \code{closed} slot. }

    \item{\code{closed<-}:}{
      Replacement accessor for \code{closed} slot. See the example
      below. See description of the \code{closed} slot above for details
      on how one- or two-element logical vectors are interpreted.
    }
    
    \item{\code{coerce}:}{
      Coercion methods are provided for converting back and forth
      between \code{Intervals} and \code{Intervals_full}
      objects. See example below. An error will be generated when
      attemption to down-class a \code{Intervals_full} object which
      does not have the same closure settings for every interval.
      
      A coercion method is also provided for pretty character strings.
    }
    
    \item{\code{head}:}{ A method for \code{\link{head}}. }
    
    \item{\code{rbind}:}{
      The \pkg{intervals} package provides \code{rbind} methods for its
      objects (which do exactly what you would expect), but to do so,
      must redefine the \code{rbind} argument list to permit a valid
      signature. The generic function so defined currently passes all
      other argument types to \code{base::rbind}. This may not work well
      with other packages that also attempt to define methods for
      \code{rbind}.
    }
    
    \item{\code{show}:}{ For pretty display of objects. }
    
    \item{\code{t}:}{ A method for \code{\link{t}}. }
    
    \item{\code{tail}:}{ A method for \code{\link{tail}}. }
    
    \item{\code{type}:}{ Accessor for \code{type} slot. }

    \item{\code{type<-}:}{ Replacement accessor for \code{type} slot.  }
    
  }
}

\note{
  We do not currently permit an integer data type for the endpoints
  matrix, even when \code{type = "Z"}, because this creates
  complications when taking complements -- which is most easily handled
  through the use of \code{-Inf} and \code{Inf}. This is particularly
  awkward for objects of class \code{Intervals}, since current endpoint
  closure settings may not permit inclusion of the minimal/maximal
  integer. This issue may be addressed, however, in future updates.

  Recycling is supported for the \code{closed} slot when creating
  new objects. Note also that we do not require endpoints which are of
  data type \code{integer} for objects with \code{type = "Z"}, nor of
  data type \code{numeric} for objects with \code{type = "R"}. We do
  not, however, permit non-integral values for \code{numeric} endpoints
  if \code{type = "Z"}.
}

\section{Warning}{
  Validity checking takes place when, for example, using the
  \code{type<-} replacement accessor: if one attempts to set type to
  \code{"Z"} but the endpoint matrix contains non-integer values, an
  error is generated. Because accessors are not used for the endpoint
  matrix itself, though, it is possible to create invalid \code{"Z"}
  objects by setting endpoints to inappropriate values.
}

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

j <- as( i, "Intervals_full" )
closed(j)[ 3:4, 2 ] <- FALSE
closed(j)[ 4, 1 ] <- FALSE

print(j)

k <- new( "Intervals_full", matrix( 1:6, 3 ), closed = c( TRUE, FALSE ) )
}

\keyword{classes}
