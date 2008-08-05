setGeneric( "distance_to_nearest", function( from, to ) standardGeneric( "distance_to_nearest" ) )

setMethod(
          "distance_to_nearest",
          signature( "numeric", "Intervals_virtual" ),
          function( from, to ) {
            if ( nrow(to) == 0 ) return( rep( as.numeric( NA ), length( from ) ) )
            # Close, collapse and sort
            to <- interval_union( if ( type(to) == "Z" ) close_intervals(to) else to )
            # Create interpolating function
            n <- nrow(to)
            gap_x <- ( to[ -1, 1 ] + to[ -n, 2 ] ) / 2
            gap_y <- ( to[ -1, 1 ] - to[ -n, 2 ] ) / 2
            f <- approxfun( c( to, gap_x ), c( rep( 0, n*2 ), gap_y ) )
            # Compute results
            below <- from < to[1,1]
            above <- from > to[n,2]
            result <- f( from )
            result[ below ] <- to[1,1] - from[ below ]
            result[ above ] <- from[ above ] - to[n,2]    
            names( result ) <- names( from )
            return( result )
          }
          )

setMethod(
          "distance_to_nearest",
          signature( "Intervals_virtual", "Intervals_virtual" ),
          function( from, to ) {
            overlapped <- sapply( interval_overlap( to, from ), length ) > 0
            if ( type(from) == "Z" ) from <- close_intervals(from)
            result <- pmin(
                           distance_to_nearest( from[,1], to ),
                           distance_to_nearest( from[,2], to )
                           )
            result[ overlapped ] <- 0
            return( result )
          }
          )
