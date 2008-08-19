# Match the Biobase combine argument list. Note that my own code would be much
# cleaner without the unnecessary y argument, but we have to have it to match
# Biobase.

setGeneric( "combine", function( x, y, ... ) standardGeneric( "combine" ) )

setMethod(
          "combine",
          signature( "Intervals" ),
          function( x, y, ... ) {
            args <- c( if ( missing(y) ) list() else list(y), list(...) )
            if ( length( args ) > 0 ) {
              args <- args[ !sapply( args, is.null ) ]
              if ( !all( sapply( args, is, "Intervals" ) ) )
                stop( "All arguments should be of the same class." )
              if ( !all( sapply( args, type ) == type(x) ) )
                stop( "All arguments should have the same 'type' slot." )
              if ( !all( sapply( args, function(y) identical( closed(x), closed(y) ) ) ) )
                if( type(x) == "Z" )
                  args <- lapply( args, close_intervals, left = closed(x)[1], right = closed(x)[2] )
                else
                  stop( "All arguments should have the same 'closed' slot." )
              x@.Data <- do.call( rbind, c( list(x), args ) )
            }
            return(x)
          }
          )

setMethod(
          "combine",
          signature( "Intervals_full" ),
          function( x, y, ... ) {
            # TO-DO: coerce Intervals objects up if required.
            args <- c( if ( missing(y) ) list() else list(y), list(...) )
            if ( length( args ) > 0 ) {
              args <- args[ !sapply( args, is.null ) ]
              if ( !all( sapply( args, is, "Intervals_full" ) ) )
                stop( "All arguments should be of the same class." )              
              if ( !all( sapply( args, type ) == type(x) ) )
                stop( "All arguments should have the same 'type' slot." )
              x@.Data <- do.call( rbind, c( list(x), args ) )
              closed(x) <- do.call(
                                   rbind,
                                   c(
                                     list( closed(x) ),
                                     lapply( args, closed )
                                     )
                                   )
            }
            return(x)
          }
          )

setMethod(
          "combine",
          signature( "missing" ),
          function( x, y, ... ) {
            # Permitting do.call use with named lists, since do.call will put
            # elements whose names are not "x" or "y" into the ... argument.            
            args <- c( if ( missing(y) ) list() else list(y), list(...) )
            names( args ) <- NULL
            if ( length( args ) == 0 ) return ( NULL )
            else return( do.call( combine, args ) )
          }
          )
