setGeneric( "distance_to_nearest", function( from, to, ... ) standardGeneric( "distance_to_nearest" ) )

setMethod(
          "distance_to_nearest",
          signature( "numeric", "Intervals_virtual" ),
          function( from, to, check_valid = TRUE ) {
            # Collapse and sort, and close for Z.
            to <- reduce( to, check_valid )
            if ( type(to) == "Z" ) to <- close_intervals( to )
            if ( nrow(to) == 0 ) return( rep( as.numeric( NA ), length( from ) ) )
            # Create interpolating function
            n <- nrow(to)
            # gap_x is a vector of gap midpoints
            gap_x <- ( to[ -1, 1 ] + to[ -n, 2 ] ) / 2
            gap_y <- ( to[ -1, 1 ] - to[ -n, 2 ] ) / 2
            x <- c( as.vector( to ), gap_x )
            y <- c( rep( 0, n*2 ), gap_y )
            use <- !duplicated( x ) & is.finite( x )
            # Note that approxfun requires at least two distinct x values. We
            # use "rule = 2" to handle infinite endpoints properly: in the
            # preceding line they are dropped, but "rule = 2" causes extension
            # of the preceding 0 out to infinity, as desired.
            if( sum( use ) > 1 )
              f <- approxfun( x[ use ], y[ use ], rule = 2 )
            else
              f <- function(x) rep( 0, length(x) )
            # Compute results. Note that is.na( NaN ) is TRUE.
            result <- rep( as.numeric( NA ), length( from ) )
            use <- !is.na( from ) 
            below <- from < to[1,1] & use
            above <- from > to[n,2] & use
            result[ use ] <- f( from[ use ] )
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
            if ( any( empty( from ), na.rm = TRUE ) ) {
              warning( "Some empty 'from' intervals encountered. Setting to NA...", call. = FALSE )
              from[ empty(from), ] <- NA
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
