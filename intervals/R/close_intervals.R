setGeneric( "close_intervals", def = function(x) standardGeneric( "close_intervals" ) )

setMethod(
          "close_intervals",
          signature( "Intervals" ),
          function(x) {
            if ( type(x) == "R" )
              stop( "Only applicable to type 'Z'." )
            if ( !closed(x)[1] ) x[,1] <- x[,1] + 1
            if ( !closed(x)[2] ) x[,2] <- x[,2] - 1
            closed(x) <- TRUE
            # In case there were empty open intervals
            empty <- x[,2] < x[,1]
            if ( any( empty ) ) {
              warning( "Empty intervals encountered and removed." )
              x <- x[ !empty, ]
            }
            return( x )
          }
          )

setMethod(
          "close_intervals",
          signature( "Intervals_full" ),
          function(x) {
            if ( type(x) == "R" )
              stop( "Only applicable to type 'Z'." )
            closed1 <- closed(x)[,1]
            closed2 <- closed(x)[,2]
            x[!closed1,1] <- x[!closed1,1] + 1
            x[!closed2,2] <- x[!closed2,2] - 1
            closed(x) <- TRUE
            # In case there were empty open intervals
            empty <- x[,2] < x[,1]
            if ( any( empty ) ) {
              warning( "Empty intervals encountered and removed." )
              x <- x[ !empty, ]
            }
            return( x )
          }
          )

setGeneric( "open_intervals", def = function(x) standardGeneric( "open_intervals" ) )

setMethod(
          "open_intervals",
          signature( "Intervals" ),
          function(x) {
            if ( type(x) == "R" )
              stop( "Only applicable to type 'Z'." )
            if ( closed(x)[1] ) x[,1] <- x[,1] - 1
            if ( closed(x)[2] ) x[,2] <- x[,2] + 1
            closed(x) <- FALSE
            return( x )
          }
          )

setMethod(
          "open_intervals",
          signature( "Intervals_full" ),
          function(x) {
            if ( type(x) == "R" )
              stop( "Only applicable to type 'Z'." )
            closed1 <- closed(x)[,1]
            closed2 <- closed(x)[,2]
            x[closed1,1] <- x[closed1,1] - 1
            x[closed2,2] <- x[closed2,2] + 1
            closed(x) <- FALSE
            return( x )
          }
          )

