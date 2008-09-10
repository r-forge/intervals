clusters <- function( pos, w, which = FALSE ) {
  regions <- reduce( Intervals( cbind( pos - w/2, pos + w/2 ), type = "R" ) )
  clusters <- interval_overlap( pos, regions )
  clusters <- clusters[ sapply( clusters, length ) > 1 ]
  if ( which ) return( lapply( clusters, function(i) pos[i] ) )
  else return( clusters )
}
