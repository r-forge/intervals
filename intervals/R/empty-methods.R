setGeneric( "empty", def = function(x, ...) standardGeneric( "empty" ) )

setMethod(
          "empty",
          signature( "Intervals" ),
          function(x, tolerance = .Machine$double.eps^0.5 ) {
            result <- rd_equal( size(x), rep( 0, nrow(x) ), tolerance )
            # Valid objects have x[,1] <= x[,2], so...
            if( type(x) == "R" && all( closed( x ) ) )
              result[ !is.na( result ) ] <- FALSE
            return( result )
          }
          )

setMethod(
          "empty",
          signature( "Intervals_full" ),
          function(x, tolerance = .Machine$double.eps^0.5 ) {
            result <- rd_equal( size(x), rep( 0, nrow(x) ), tolerance )
            if ( type(x) == "R" )
              result[ !is_na( result ) & closed(x)[,1] & closed(x)[,2] ] <- FALSE
            return( result )
          }
          )
