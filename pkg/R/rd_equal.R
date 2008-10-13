rd_equal <- function( x, y, tolerance = .Machine$double.eps^0.5 ) {
  # Pointwise relative difference computation. RDs less than tolerance are taken
  # as equality. When either value is infinite, a simple comparison is used instead.
  if ( length(x) != length(y) )
    stop( "The 'x' and 'y' arguments should have the same length." )
  has_na <- is.na( x ) | is.na( y )
  has_inf <- !has_na & ( is.infinite( x ) | is.infinite( y ) )
  max_abs <- pmax( abs(x), abs(y) )
  zeros <- !has_na & max_abs == 0
  result <- ( abs( x - y ) / max_abs ) < tolerance
  result[ zeros ] <- TRUE
  result[ has_na ] <- NA
  result[ has_inf ] <- x[ has_inf ] == y[ has_inf ]
  return( result )
}
