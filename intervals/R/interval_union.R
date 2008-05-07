# Unions are constructed by ordering endpoints. When first and last endpoints
# coincide, correct ordering -- on the basis of whether they are open or closed,
# and first or last for their respective interval -- is required for correct
# results. This matrix establishes the correct ordering, as is used below. It's
# cute to do this in all R, but we need C for iterval_overlap anyway, so we may
# eventually want to handle this in C as well.

ordering_matrix <- matrix(
                          c( 3L, 1L, 2L, 4L ),
                          nrow = 2,
                          dimnames = list( c( "open", "closed" ), c( "first", "last" ) )
                          )

setGeneric( "interval_union", def = function( x, y, ... ) standardGeneric( "interval_union" ) )

setMethod(
          "interval_union",
          signature( "Intervals_full", "missing" ),
          function( x ) {
            # Note that if integer intervals are forced to double-open notation,
            # then the real-valued code works immediately, with no need for
            # back-conversion afterwards. This is because the open intervals
            # corresponding to two consecutive integers overlap one another.
            has_na <- is.na( x[,1] ) | is.na( x[,2] )
            if ( any( has_na ) )
              x <- x[ !has_na, ]
            if ( nrow(x) == 0 ) result <- x
            else {
              if ( type(x) == "Z" ) x <- open_intervals(x)
              closed <- as.vector( t( closed(x) ) )
              data <- data.frame(
                                 pos = as.vector( t( x@.Data ) ),
                                 ordering = ordering_matrix[ cbind( ifelse( closed, 2L, 1L ), rep( c(1L,2L), nrow( x ) ) ) ],
                                 closed = closed,
                                 score = rep( c( 1, -1 ), nrow( x ) )              
                                 )
              data <- data[ with( data, order( pos, ordering ) ), ]
              data$sum <- cumsum( data$score )
              z <- which( data$sum == 0 )
              first <- c( 1, z[ z < nrow(data) ] + 1 ) 
              result <- with(
                             data,
                             new(
                                 "Intervals_full",
                                 cbind( pos[ first ], pos[ z ] ),
                                 closed = cbind( closed[ first ], closed[ z ] ),
                                 type = type( x )
                                 )
                             )
              colnames( result ) <- colnames( x )
            }
            return( result[ !empty( result ), ] )
          }
          )

setMethod(
          "interval_union",
          signature( "Intervals", "missing" ),
          # TO DO: make this more efficient (but less clean) by not coercing
          function( x ) {
            result <- as( interval_union( as( x, "Intervals_full" ) ), "Intervals" )
            if ( type( x ) == "Z" ) {
              # We always get, but may not want, double-open results for type Z
              if ( closed(x)[1] ) result[,1] <- result[,1] + 1
              if ( closed(x)[2] ) result[,2] <- result[,2] - 1
              closed( result ) <- closed( x )
            }
            return( result )
          }
          )

setMethod(
          "interval_union",
          signature( "Intervals_virtual", "Intervals_virtual" ),
          function( x, y, ... ) {
            # Note that the rbind call will do type checking on x, y, and ...
            interval_union(
                           do.call(
                                   rbind,
                                   c( list(x, y), ... )
                                   )
                           )
          }
          )

