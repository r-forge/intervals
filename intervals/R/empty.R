# Intervals with tied endpoints and with two closed endpoint are taken to
# represent a point.

setGeneric( "empty", def = function(x) standardGeneric( "empty" ) )

setMethod(
          "empty",
          signature( "Intervals" ),
          function(x) {
            result <- size(x) <= 0
            if ( type(x) == "R" && all( closed(x) ) )
              result[ x[,1] == x[,2] ] <- FALSE
            return( result )
          }
          )

setMethod(
          "empty",
          signature( "Intervals_full" ),
          function(x) {
            result <- size(x) <= 0
            if ( type(x) == "R" )
              result[ x[,1] == x[,2] & closed(x)[,1] & closed(x)[,2] ] <- FALSE
            return( result )
          }
          )
