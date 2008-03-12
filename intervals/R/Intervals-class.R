# We define a class for two-column interval endpoint matrices. The two
# additional slots are for explicitly describing interval endpoint closure and
# for stating whether intervals are to be interpreted as being on the integers
# or the real line. Would could have just check

setClass(
         "Intervals",
         representation(
                        closed = "matrix", # A matrix with 0 rows, or of the same dimension as .Data
                        type = "character" # Either "Z" or "R"
                        ),
         prototype(
                   matrix( 0L, 0, 2 ),
                   closed = matrix( TRUE, 0, 2 ),
                   type = "Z"
                   ),
         contains = "matrix",              # A two-column interval endpoint matrix
         validity = function( object ) {
           if ( !( is.numeric( object@.Data ) && ncol( object@.Data ) == 2 )  )
             return( "An Intervals object should be a two-column numeric matrix." )
           if ( !( object@type %in% c( "R", "Z" ) ) )
             return( "The 'type' slot may only contain 'R' or 'Z', for the real line or the integers, respectively." )
           if (
               !(
                 is.logical( object@closed ) &&
                 ( nrow( object@closed ) == 0 || dim( object@.Data ) == dim( object@closed ) )
                 )
               )
             return( "The 'closed' slot should be a logical matrix with either 0 rows, or with the same dimensions as the main Intervals matrix." )
           if ( object@type == "Z" && !all( object@.Data %% 1 == 0 ) )
             return( "Non-integer endpoints found, but 'type' set to 'Z'." )
           # Passed
           return( TRUE )
         }
         )

