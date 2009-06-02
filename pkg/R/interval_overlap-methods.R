setGeneric( "interval_overlap", def = function( from, to, ... ) standardGeneric( "interval_overlap" ) )

setMethod(
          "interval_overlap",
          signature( "Intervals_virtual_or_numeric", "Intervals_virtual_or_numeric" ),
          function( from, to, check_valid = TRUE ) {
            result <- which_nearest( from, to, check_valid )$which_overlap
            names( result ) <- rownames( from )
            return( result )
          }
          )
