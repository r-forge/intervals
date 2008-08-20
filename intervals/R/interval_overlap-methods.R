setGeneric( "interval_overlap", def = function( query, target, ... ) standardGeneric( "interval_overlap" ) )

setMethod(
          "interval_overlap",
          signature( "Intervals_virtual", "Intervals_virtual" ),
          function(
                   query,
                   target,
                   tolerance = .Machine$double.eps^0.5,
                   check_valid = TRUE
                   )
          {
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
                            class(query) == "Intervals_full", class(target) == "Intervals_full",
                            tolerance
                            )
            names( result ) <- rownames( target )
            return( result )
          }
          )
