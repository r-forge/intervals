setGeneric( "close_intervals", def = function(x, ...) standardGeneric( "close_intervals" ) )

setMethod(
          "close_intervals",
          signature( "Intervals" ),
          function(x, left = TRUE, right = TRUE) {
            if ( type(x) == "R" )
              stop( "Only applicable to type 'Z'." )
            if ( !closed(x)[1] && left ) x[,1] <- x[,1] + 1
            if ( !closed(x)[2] && right ) x[,2] <- x[,2] - 1
            closed(x) <- c( left, right )
            if ( any( empty(x) ) ) {
              warning( "Empty intervals encountered and removed." )
              x <- x[ !empty(x), ]
            }
            return( x )
          }
          )

setMethod(
          "close_intervals",
          signature( "Intervals_full" ),
          function(x, left = TRUE, right = TRUE) {
            if ( type(x) == "R" )
              stop( "Only applicable to type 'Z'." )
            if ( left )
              x[!closed(x)[,1],1] <- x[!closed(x)[,1],1] + 1
            if ( right )
              x[!closed(x)[,2],2] <- x[!closed(x)[,2],2] - 1
            closed(x) <- c( left, right )
            if ( any( empty(x) ) ) {
              warning( "Empty intervals encountered and removed." )
              x <- x[ !empty(x), ]
            }
            return( x )
          }
          )

setGeneric( "open_intervals", def = function(x, ...) standardGeneric( "open_intervals" ) )

setMethod(
          "open_intervals",
          signature( "Intervals" ),
          function(x, left = TRUE, right = TRUE) {
            if ( type(x) == "R" )
              stop( "Only applicable to type 'Z'." )
            if ( closed(x)[1] && left ) x[,1] <- x[,1] - 1
            if ( closed(x)[2] && right ) x[,2] <- x[,2] + 1
            closed(x) <- c( !left, !right )
            return( x )
          }
          )

setMethod(
          "open_intervals",
          signature( "Intervals_full" ),
          function(x, left = TRUE, right = TRUE) {
            if ( type(x) == "R" )
              stop( "Only applicable to type 'Z'." )
            if ( left )
              x[closed(x)[,1],1] <- x[closed(x)[,1],1] - 1
            if ( right )
              x[closed(x)[,2],2] <- x[closed(x)[,2],2] + 1
            closed(x) <- c( !left, !right )
            return( x )
          }
          )

