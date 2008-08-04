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
  return( result )
}
