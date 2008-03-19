# We define two classes for two-column interval endpoint matrices. The basic class
# has a two-element boolean vector indicating whether endpoints are closed or
# not. The full class caries a matrix with one boolean per endpoint, permitting
# full control.




######## Virtual

# (R 2.6.2) If I add the "VIRTUAL" tag to the representation, I am not able to
# extend this class! I believe this arises because I am already extending a base
# class, but I am not sure. This tag would be more appropriate, but I leave it
# off... 

setClass(
         "Intervals_virtual",
         representation( type = "character" ),
         prototype(
                   matrix( 0, 0, 2 ),
                   type = "R"
                   ),
         contains = "matrix",
         validity = function( object ) {
           # Check main matrix
           if ( !is.numeric( object@.Data ) || ncol( object@.Data ) != 2  )
             return( "The 'Intervals' classes are based on two-column, numeric matrices." )
           # Check 'type' string
           if ( length( object@type ) != 1 || !( object@type %in% c( "Z", "R" ) ) )
             return( "The 'type' slot should be 'Z' or 'R'." )
           # For type 'Z', check for integral endpoints
           if ( object@type == "Z" && !all( object@.Data %% 1 == 0 ) )
             return( "Non-integer-valued endpoints not permitted for type 'Z'." )
           # Check for valid intervals
           if ( any( object@.Data[,2] < object@.Data[,1] ) )
             return( "One or more intervals with second endpoint before first." )
           return( TRUE )
         }
         )

# Accessors and replacement methods for virtual class

setGeneric( "closed", function(x) standardGeneric( "closed" ) )

setMethod(
          "closed",
          signature( "Intervals_virtual" ),
          function( x ) x@closed
          )

setGeneric( "closed<-", function( x, value ) standardGeneric( "closed<-" ) )

setReplaceMethod(
                 "closed", "Intervals_virtual",
                 function( x, value ) {
                   # For recycling
                   x@closed[ 1:length(x@closed) ] <- value
                   return(x)
                 }
                 )

setGeneric( "type", function(x) standardGeneric( "type" ) )

setMethod(
          "type",
          signature( "Intervals_virtual" ),
          function( x ) x@type
          )

setGeneric( "type<-", function( x, value ) standardGeneric( "type<-" ) )

setReplaceMethod(
                 "type", "Intervals_virtual",
                 function( x, value ) {
                   if ( length( value ) != 1 || !( value %in% c( "Z", "R" ) ) )
                     stop( "The 'type' slot should be 'Z' or 'R'." )
                   if ( value == "Z" && !all( x@.Data %% 1 == 0 ) )
                     stop( "Non-integer-valued endpoints not permitted for type 'Z'." )
                   x@type <- value
                   return(x)
                 }
                 )




######## Intevals

# Common endpoint closure state for all intervals

setClass(
         "Intervals",
         representation( closed = "logical" ),
         prototype( closed = c( TRUE, TRUE ) ),
         contains = "Intervals_virtual",
         validity = function( object ) {
           # Check 'closed' slot
           if ( length( object@closed ) != 2 )
             return( "The 'closed' slot should be a logical of length 2." )
           return( TRUE )
         }
         )

setMethod(
          "[",
          signature( "Intervals" ),
          function( x, i, j, ..., drop ) {
            if ( missing(i) ) i <- TRUE
            if ( missing(j) ) {
              # Preserve class. Note that both [i,] and [i] syntax subset rows.
              x@.Data <- x@.Data[i,,drop=FALSE]
              return( x )
            }
            else return( x@.Data[i,j] )
          }
          )

setGeneric(
           "rbind",
           function( x, ... ) standardGeneric( "rbind" ),
           # We need a different generic argument set. Note that this is
           # currently throwning warnings on first execution. See if this
           # persists once the package namespace is set up properly.
           useAsDefault = function( x, ... ) base::rbind( x, ... )
           ) 

setMethod(
          "rbind",
          signature( "Intervals_virtual" ),
          function( x, ... ) {
            args <- list(...)
            if ( length( args ) > 0 ) {
              if ( !all( sapply( args, function(y) class(x) == class(y) ) ) )
                stop( "All argments should be of the same class." )
              if ( !all( sapply( args, function(y) type(x) == type(y) ) ) )
                stop( "All argments should have the same 'type' slot." )
              x@.Data <- do.call( rbind, c( list(x@.Data), lapply( args, function(y) y@.Data ) ) )
            }
            return(x)
          }
          )

setMethod(
          "rbind",
          signature( "Intervals" ),
          function( x, ... ) {
            args <- list(...)
            if ( length( args ) > 0 && !all( sapply( args, function(y) identical( closed(x), closed(y) ) ) ) )
              stop( "All argments should have the same 'closed' slot." )
            callNextMethod( x, ... )
          }
          )

setMethod(
          "rbind",
          signature( "Intervals_full" ),
          function( x, ... ) {
            args <- list(...)
            if ( length( args ) > 0 )
              x@closed <- rbind( closed(x), do.call( rbind, lapply( args, function(y) closed(y) ) ) )
            callNextMethod( x, ... )
          }
          )




######## Intervals_full

# Full control of endpoint closure. Note that if the 'closed' slot is omitted,
# we use an 'initialize' method to create an appropriately sized matrix of TRUE
# values. We also permit vector input, with recycling, for the 'closed' slot.

setClass(
         "Intervals_full",
         representation( closed = "matrix" ),
         prototype( closed = matrix( TRUE, 0, 2 ) ),
         contains = "Intervals_virtual",
         validity = function( object ) {
           # Check 'closed' slot
           if ( !is.logical( object@closed ) || dim( object@.Data ) != dim( object@closed ) )
             return( "The 'closed' slot should be a logical matrix with the same dimensions as the main endpoints matrix." )
           return( TRUE )
         }
         )

setMethod(
          "initialize",
          signature( "Intervals_full" ),
          function( .Object, .Data, type, closed ) {
            if ( !missing( .Data ) ) .Object@.Data <- .Data
            if ( !missing( type ) ) .Object@type <- type
            # Careful here. Since the closed slot initializes to 0 rows, we
            # should not used the "closed<-" method yet.
            .Object@closed <- matrix( TRUE, nrow( .Object ), 2 )
            if ( !missing( closed ) )
              # Now it's OK though, for recycling
              closed( .Object ) <- closed
            if ( validObject( .Object ) ) return( .Object )
          }
          )

setMethod(
          "[",
          signature( "Intervals_full" ),
          function( x, i, j, ..., drop ) {
            if ( missing(i) ) i <- TRUE
            if ( missing(j) ) {
              # Preserve class. Note that both [i,] and [i] syntax subset rows.
              if ( is.character(i) ) i <- match( i, rownames( x ) )
              x@.Data <- x@.Data[i,,drop=FALSE]
              x@closed <- x@closed[i,,drop=FALSE]
              return( x )
            }
            else return( x@.Data[i,j] )
          }
          )
                 



######## Coercion

setMethod(
          "coerce",
          signature( from = "Intervals", to = "Intervals_full" ),
          function( from, to, strict ) {
            new(
                "Intervals_full",
                from@.Data,
                type = from@type,
                closed = cbind(
                  rep( from@closed[1], nrow( from ) ),
                  from@closed[2]
                  )
                )
          }
          )

setMethod(
          "coerce",
          signature( from = "Intervals_full", to = "Intervals" ),
          function( from, to, strict ) {
            if ( !all( t( from@closed ) == from@closed[1,] ) )
              stop( "Intervals do not all have the same endpoint closure." )
            new(
                "Intervals",
                from@.Data,
                type = from@type,
                closed = from@closed[1,]
                )
          }
          )
