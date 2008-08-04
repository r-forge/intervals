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
