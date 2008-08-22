# We define two classes for two-column interval endpoint matrices. The basic class
# has a two-element boolean vector indicating whether endpoints are closed or
# not. The full class caries a matrix with one boolean per endpoint, permitting
# full control.




######## Class definitions

#### Virtual

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
           if ( !is.double( object@.Data ) || ncol( object@.Data ) != 2  )
             return( "The 'Intervals' classes are based on two-column, numeric matrices." )
           # Check 'type' string
           if ( length( object@type ) != 1 || !( object@type %in% c( "Z", "R" ) ) )
             return( "The 'type' slot should be 'Z' or 'R'." )
           # For type 'Z', check for integral endpoints
           if ( object@type == "Z" && !all( object@.Data[ is.finite( object@.Data ) ] %% 1 == 0 ) )
             return( "Non-integer-valued endpoints not permitted for type 'Z'." )
           # Check for valid intervals
           if ( any( object@.Data[,2] < object@.Data[,1], na.rm = TRUE ) )
             return( "One or more intervals with second endpoint before first." )
           return( TRUE )
         }
         )

setMethod(
          "initialize",
          signature( "Intervals_virtual" ),
          function( .Object, .Data, ... ) {
            if ( missing( .Data ) ) callNextMethod( .Object, ... )
            else {
              if ( is.data.frame( .Data ) )
                .Data <- as.matrix( .Data )
              if ( !is.matrix( .Data ) )
                .Data <- matrix( .Data, ncol = 2 )
              if ( is.integer( .Data ) ) {
                # warning( "Converting endpoints from 'integer' to 'numeric' data type. See class documentation.", call. = FALSE )
                .Data <- matrix( as.numeric( .Data ), nrow( .Data ), ncol( .Data ) )
              }
              callNextMethod( .Object, .Data, ... )
            }
          }
          )

#### Intervals

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
          "initialize",
          signature( "Intervals" ),
          function( .Object, .Data, closed, ... ) {
            if ( missing( closed ) ) callNextMethod( .Object, .Data, ... )
            else {
              if ( length( closed ) == 1 ) closed <- c( closed, closed )
              callNextMethod( .Object, .Data, closed = closed, ... )
            }
          }
          )

#### Intervals_full

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
          function( .Object, .Data, closed, ... ) {
            if ( !missing( .Data ) ) {
              if ( !is.matrix( .Data ) )
                .Data <- matrix( .Data, ncol = 2 )
              if ( missing( closed ) )
                closed <- matrix( TRUE, nrow( .Data ), 2 )
              if ( is.vector( closed ) ) {
                if ( length( closed ) > 2 )
                  stop( "The 'closed' argument should be a matrix, or a vector of length 1 or 2." )
                closed <- matrix(
                                 if ( nrow( .Data ) == 0 ) logical() else closed,
                                 nrow( .Data ),
                                 2,
                                 byrow = TRUE
                                 )
              }
              callNextMethod( .Object, .Data, closed = closed, ... )
            }
            else callNextMethod( .Object, ... )
          }
          )




######## Convenience functions

Intervals <- function( .Data, ... )
  new( "Intervals", .Data, ... )

Intervals_full <- function( .Data, closed, type )
  new( "Intervals_full", .Data, ... )




######## Subsetting

setMethod(
          "[",
          signature( "Intervals" ),
          function( x, i, j, ..., drop ) {
            if ( missing(i) ) i <- rep( TRUE, nrow(x) )
            if ( missing(j) ) {
              # Preserve class. Note that both [i,] and [i] syntax subset rows.
              x@.Data <- x@.Data[i,,drop=FALSE]
              return( x )
            }
            else return( x@.Data[i,j] )
          }
          )

setMethod(
          "[",
          signature( "Intervals_full" ),
          function( x, i, j, ..., drop ) {
            if ( missing(i) ) i <- rep( TRUE, nrow(x) )
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
                type = type( from ),
                closed = cbind(
                  rep( closed(from)[1], nrow( from ) ),
                  rep( closed(from)[2], nrow( from ) )
                  )
                )
          }
          )

setMethod(
          "coerce",
          signature( from = "Intervals_full", to = "Intervals" ),
          function( from, to, strict ) {
            if ( nrow( from ) == 0 ) new_closed <- rep( TRUE, 2 )
            else new_closed <- closed( from )[1,]
            if ( !all( t( closed( from ) ) == new_closed ) )
              stop( "Intervals do not all have the same endpoint closure." )
            new(
                "Intervals",
                from@.Data,
                type = type( from ),
                closed = new_closed
                )
          }
          )

setMethod(
          "coerce",
          signature( from = "Intervals_virtual", to = "character" ),
          function( from, to, strict ) {
            if ( nrow( from ) == 0 )
              return( character() )
            else {
              cl <- closed( from )
              # So we only write main code once
              if ( is( from, "Intervals" ) )
                cl <- matrix( cl, nrow(from), 2, byrow = TRUE )
              result <- paste(
                              ifelse( cl[,1], "[", "(" ),
                              from[,1], ", ", from[,2],
                              ifelse( cl[,2], "]", ")" ),
                              sep = ""
                              )
              names( result ) <- rownames( from )
              return( result )
            }
          }
          )
