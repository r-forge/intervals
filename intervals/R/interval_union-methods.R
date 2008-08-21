setGeneric( "interval_union", def = function( x, ... ) standardGeneric( "interval_union" ) )

setMethod(
          "interval_union",
          signature( "Intervals_virtual" ),
          function( x, ..., tolerance = .Machine$double.eps^0.5, check_valid = TRUE ) {
            reduce( combine( x, ... ), tolerance, check_valid )
          }
          )

setMethod(
          "interval_union",
          signature( "missing" ),
          function( x, ..., tolerance = .Machine$double.eps^0.5, check_valid = TRUE ) {
            # Permitting do.call use with named lists, since do.call will put
            # elements whose names are not "x" into the ... argument. Stripping
            # names, however, puts arguments in place positionally.
            args <- list(...)
            names( args ) <- NULL
            if ( length( args ) == 0 ) return ( NULL )
            else
              return(
                     do.call(
                             interval_union,
                             c( args, list( tolerance = tolerance, check_valid = check_valid ) )
                             )
                     )
          }
          )

