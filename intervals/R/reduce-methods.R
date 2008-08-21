setGeneric( "reduce", def = function( x, ... ) standardGeneric( "reduce" ) )

setMethod(
          "reduce",
          signature( "Intervals_virtual" ),
          function( x, tolerance = .Machine$double.eps^0.5, check_valid = TRUE ) {
            if ( check_valid ) validObject( x )
            has_na <- is.na( x[,1] ) | is.na( x[,2] )
            if ( any( has_na ) ) {
              warning( "Intervals with NA endpoints removed.", call. = FALSE )
              x <- x[ !has_na, ]
            }
            # In order to collapse over abutting intervals
            if ( type(x) == "Z" ) x <- open_intervals( x )
            result <- .Call(
                            "_reduce",
                            x@.Data,
                            closed( x ),
                            FALSE,
                            tolerance
                            )
            result <- new( class(x), result[[1]], closed = result[[2]], type = type(x) )
            if ( type(x) == "Z" && class(x) == "Intervals" )
              result <- adjust_closure( result, close_left = closed(x)[1], close_right = closed(x)[2] )
            return( result )
          }
          )
