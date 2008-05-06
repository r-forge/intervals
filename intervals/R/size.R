# For R, size is Lebesgue measure, so closure is irrelevant.

setGeneric( "size", def = function( x, ... ) standardGeneric( "size" ) )

setMethod(
          "size",
          signature( "Intervals" ),
          function( x, as = type(x) ) {
            result <- x[,2] - x[,1]            
            if ( as == "Z" ) {
              ties <- x[,2] == x[,1]
              result[ ties ] <- ifelse( all( closed(x) ), 1, 0 )
              result[ !ties ] <- result[ !ties ] + sum( closed(x) ) - 1
            }
            return( result )
          }
          )

setMethod(
          "size",
          signature( "Intervals_full" ),
          function( x, as = type(x) ) {
            result <- x[,2] - x[,1]            
            if ( as == "Z" ) {
              ties <- x[,2] == x[,1]
              result[ ties ] <- ifelse( closed(x)[,1] & closed(x)[,2], 1, 0 )
              result[ !ties ] <- result[ !ties ] + rowSums( closed(x) )[ !ties ] - 1
            }
            return( result )
          }
          )
