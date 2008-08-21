setGeneric( "empty", def = function(x, ...) standardGeneric( "empty" ) )

setMethod(
          "empty",
          signature( "Intervals" ),
          function(x, tolerance = .Machine$double.eps^0.5 ) {
            result <- rep( FALSE, nrow(x) )
            result[ is.na( x[,1] ) | is.na( x[,2] ) ] <- NA
            if ( !all( closed(x) ) ) {
              # Valid objects have x[,1] <= x[,2], so we only check this case.
              result[ rd_equal( x[,1], x[,2], tolerance ) ] <- TRUE
              if ( type(x) == "Z" && !any( closed( x ) ) )
                result[ rd_equal( x[,1] + 1, x[,2], tolerance ) ] <- TRUE
            }
            return( result )
          }
          )

setMethod(
          "empty",
          signature( "Intervals_full" ),
          function(x, tolerance = .Machine$double.eps^0.5 ) {
            result <- rep( FALSE, nrow(x) )
            result[ is.na( x[,1] ) | is.na( x[,2] ) ] <- NA
            any_open <- !( closed(x)[,1] & closed(x)[,2] )
            both_open <- !closed(x)[,1] & !closed(x)[,2] 
            result[ any_open & rd_equal( x[,1], x[,2], tolerance ) ] <- TRUE
            if ( type(x) == "Z" )
                result[ both_open & rd_equal( x[,1] + 1, x[,2], tolerance ) ] <- TRUE
            return( result )          
          }
          )
