# Unions are constructed by ordering endpoints. When first and last endpoints
# coincide, correct ordering -- on the basis of whether they are open or closed,
# and first or last for their respective interval -- is required for correct
# results.

setGeneric( "interval_union", def = function( x, ... ) standardGeneric( "interval_union" ) )

setMethod(
          "interval_union",
          signature( "Intervals_virtual" ),
          function( x, ..., tolerance = .Machine$double.eps^0.5, check_valid = TRUE ) {
            x_comb <- combine( x, ... )
            if ( check_valid ) validObject( x_comb )
            has_na <- is.na( x_comb[,1] ) | is.na( x_comb[,2] )
            if ( any( has_na ) ) {
              warning( "Intervals with NA endpoints removed.", call. = FALSE )
              x_comb <- x_comb[ !has_na, ]
            }
            if ( type(x) == "Z" ) x_comb <- open_intervals( x_comb )
            result <- .Call(
                            "_interval_union",
                            x_comb@.Data,
                            closed( x_comb ),
                            FALSE,
                            tolerance
                            )
            result <- new( class(x), result[[1]], closed = result[[2]], type = type(x) )
            if ( type(x) == "Z" && class(x) == "Intervals" )
              result <- adjust_closure( result, close_left = closed(x)[1], close_right = closed(x)[2] )
            return( result )
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

