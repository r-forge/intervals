setGeneric( "interval_intersection", def = function( x, ... ) standardGeneric( "interval_intersection" ) )

setMethod(
          "interval_intersection",
          signature( "Intervals_virtual" ),
          function( x, ... ) {
            args <- c( list(x), list(...) )
            complements <- lapply( args, interval_complement )
            interval_complement( do.call( interval_union, complements ) )
          }
          )

setMethod(
          "interval_intersection",
          signature( "missing" ),
          function( x, ... ) {
            # Permitting do.call use with named lists, since do.call will put
            # elements whose names are not "x" into the ... argument. Stripping
            # names, however, puts arguments in place positionally.
            args <- list(...)
            names( args ) <- NULL
            if ( length( args ) == 0 ) return ( NULL )
            else return( do.call( interval_intersection, args ) )
          }
          )
