rd_equal <- function( x, y, tolerance = .Machine$double.eps^0.5 ) {
  # Pointwise relative difference computation. RDs less than tolerance are taken
  # as equality.
  if ( length(x) != length(y) )
    stop( "The 'x' and 'y' arguments should have the same length." )
  max_abs <- pmax( abs(x), abs(y) )
  result <- rep( FALSE, length(x) )
  zeros <- max_abs == 0
  result[ zeros ] <- TRUE
  result[ !zeros ] <- ( abs( x - y ) / max_abs )[ !zeros ] < tolerance
}

setGeneric( "empty", def = function(x, ...) standardGeneric( "empty" ) )

setMethod(
          "empty",
          signature( "Intervals" ),
          function(x, tolerance = .Machine$double.eps^0.5 ) {
            rd_equal( x[,1], x[,2], tolerance ) & !all( closed(x) )
          }
          )

setMethod(
          "empty",
          signature( "Intervals_full" ),
          function(x, tolerance = .Machine$double.eps^0.5 ) {
            rd_equal( x[,1], x[,2], tolerance ) & !( closed(x)[,1] & closed(x)[,2] )
          }
          )
