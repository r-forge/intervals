######## which_nearest

setGeneric( "which_nearest", function( from, to, ... ) standardGeneric( "which_nearest" ) )

setMethod(
          "which_nearest",
          signature( "numeric", "Intervals_virtual" ),
          function( from, to, check_valid = TRUE ) {
            # Be careful not to drop rows -- even empty rows -- of the 'to' object.
            if ( nrow(to) == 0 ) return( lapply( 1:length( from ), function(x) integer() ) )
            to[ empty( to ), ] <- NA
            if ( type(to) == "Z" )
              to <- close_intervals( to )
            else
              closed( to ) <- TRUE
            from <- new(
                        class( to ),
                        cbind( from, from ),
                        closed = TRUE,
                        type = type( to )
                        )
            interval_overlap(
                             expand( from, distance_to_nearest( from, to, check_valid ) ),
                             to
                             )
          }
          )

