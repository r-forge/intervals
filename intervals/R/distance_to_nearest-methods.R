setGeneric( "distance_to_nearest", function( from, to, ... ) standardGeneric( "distance_to_nearest" ) )

setMethod(
          "distance_to_nearest",
          signature( "numeric", "Intervals_virtual" ),
          function( from, to, check_valid = TRUE ) {
            if ( nrow(to) == 0 ) return( rep( as.numeric( NA ), length( from ) ) )
            # Close, collapse and sort
            to <- reduce( if ( type(to) == "Z" ) close_intervals(to) else to, check_valid )
            # Create interpolating function
            n <- nrow(to)
            gap_x <- ( to[ -1, 1 ] + to[ -n, 2 ] ) / 2
            gap_y <- ( to[ -1, 1 ] - to[ -n, 2 ] ) / 2
            x <- c( to, gap_x )
            y <- c( rep( 0, n*2 ), gap_y )
            use <- !duplicated( x ) & is.finite( x )
            # Note that approxfun requires at least two distinct x values. We
            # use "rule = 2" to handle infinite endpoints properly: in the
            # preceding line they are dropped, but "rule = 2" causes extension
            # of the preceding 0 out to infinity, as desired.
            if( sum( use ) > 1 )
              f <- approxfun( x[ use ], y[ use ], rule = 2 )
            else
              f <- function(x) 0
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
          function( from, to, check_valid = TRUE ) {
            if ( check_valid ) {
              validObject( from )
              validObject( to )
            }
            overlapped <- sapply( interval_overlap( from, to, check_valid = FALSE ), length ) > 0
            if ( type(from) == "Z" ) from <- close_intervals(from)
            result <- pmin(
                           distance_to_nearest( from[,1], to, check_valid = FALSE ),
                           distance_to_nearest( from[,2], to, check_valid = FALSE )
                           )
            result[ overlapped ] <- 0
            return( result )
          }
          )
