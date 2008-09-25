setGeneric( "interval_overlap", def = function( query, target, ... ) standardGeneric( "interval_overlap" ) )

setMethod(
          "interval_overlap",
          signature( "Intervals_virtual", "Intervals_virtual" ),
          function( query, target, check_valid = TRUE ) {
            if ( check_valid && !( validObject(query) && validObject(target) ) )
              stop( "The 'query' and/or 'target' objects are invalid." )
            if ( type(query) != type(target) )
              stop( "Both 'query' and 'target' should have the same type." )
            if ( any( empty( query ), na.rm = TRUE ) ) {
              warning( "Some empty query intervals encountered. Setting to NA...", call. = FALSE )
              query[ empty(query), ] <- NA
            }
            if ( any( empty( target ), na.rm = TRUE ) ) {
              warning( "Some empty target intervals encountered. Setting to NA...", call. = FALSE )
              target[ empty(target), ] <- NA
            }
            result <- .Call(
                            "_interval_overlap",
                            query@.Data, target@.Data,
                            closed(query), closed(target),
                            class(query) == "Intervals_full", class(target) == "Intervals_full"
                            )
            names( result ) <- rownames( target )
            return( result )
          }
          )

setMethod(
          "interval_overlap",
          signature( "numeric", "Intervals_virtual" ),
          function( query, target, check_valid = TRUE ) {
            if ( check_valid && !validObject(target) )
              stop( "The 'target' object is invalid." )
            if ( any( empty( target ), na.rm = TRUE ) ) {
              warning( "Some empty target intervals encountered. Setting to NA...", call. = FALSE )
              target[ empty(target), ] <- NA
            }            
            if ( type( target ) == "Z" ) {
              non_int <- ( query %% 1 != 0 )
              if ( any( non_int, na.rm = TRUE ) ) {
                warning( "The 'target' object is of type 'Z'. Setting non-integer values in 'query' to NA.", call. = FALSE )
                query[ non_int ] <- NA
              }
            }
            result <- .Call(
                            "_interval_overlap",
                            cbind( query, query ), target@.Data,
                            c( TRUE, TRUE ), closed(target),
                            class(query) == "Intervals", class(target) == "Intervals_full"
                            )
            names( result ) <- rownames( target )
            return( result )
          }
          )
