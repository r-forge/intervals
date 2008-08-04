setGeneric( "distance_to_nearest", function( x, y ) standardGeneric( "distance_to_nearest" ) )

setMethod(
          "distance_to_nearest",
          signature( "numeric", "Intervals_virtual" ),
          function( x, y ) {
            if ( nrow(y) == 0 ) return( rep( as.numeric( NA ), length( x ) ) )
            # Close, collapse and sort
            y <- interval_union( if ( type(y) == "Z" ) close_intervals(y) else y )
            # Create interpolating function
            browser()
            n <- nrow(y)
            gap_x <- ( y[ -1, 1 ] + y[ -n, 2 ] ) / 2
            gap_y <- ( y[ -1, 1 ] - y[ -n, 2 ] ) / 2
            f <- approxfun( c( y, gap_x ), c( rep( 0, n*2 ), gap_y ) )
            # Compute results
            below <- x < y[1,1]
            above <- x > y[n,2]
            result <- f( x )
            result[ below ] <- y[1,1] - x[ below ]
            result[ above ] <- x[ above ] - y[n,2]    
            return( result )
          }
          )
