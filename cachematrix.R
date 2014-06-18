##________________________________________________________________________
## R-Programming-course@coursera
## Programming Assignment 2:
## Lexical Scoping
## (peer assessment)
##
## JUN-2014
##________________________________________________________________________



##________________________________________________________________________
## :: Adopted Coding Conventions ::
##
## Variable name:
## - do NOT use underscores ( _ ) or hyphens ( - )
## - all lower case letters with words separated with dots, e.g., variable.name
## - double dot prefix for variables, in a closure,
##   is being used from a child environment
## | GOOD: msg.prefix | BAD: msgPrefix | BAD: msg_prefix
## | ..x.inv indicates usage by a child environment (e.g., inner function) 
##
## Function name:
## - camelCase (lower case letters with words separated with capital letter),
##   e.g., functionName
## | GOOD: makeCacheMatrix | BAD: MakeCacheMatrix | BAD: make_cache_matrix
##
## Constants:
## - named like functions but with an initial "k."
##________________________________________________________________________



##________________________________________________________________________
# cleanup
# removes all objects except for functions:
rm( list = setdiff( ls(), lsf.str() ) )



##________________________________________________________________________
## Return a closure with:
## - the matrix 'x' (formal argument)
## - the inverse of 'x'
## - the setter and getter functions of 'x' and its inverse
##   i.e., set(x), setInv(x.inv), get(), getInv()
##
## Arguments:
## - 'x', the matrix to cache (from which to compute the inverse ONLY ONCE)
## - 'send.message', a boolean (logical) to indicate if a message is to be
##   presented whenever the cache is being used 
##
## Pre-condition:
## - formal argument is square matrix
##
## The setter functions ensure that:
## - nothing changes when the "new" matrix is equal to the cached one
## - when a matrix changes its inverse is set to NULL
## (hence the matrix '..x' and its inverse '..x.inv' are always consistent)
##
## The getter functions ensure that:
## - the inverse of a matrox is computed only once
## - a message is sent when cached data is being used
makeCacheMatrix <- function( x = matrix(), send.message = FALSE ) {
   assertSquareMatrix( x )
   ..x <- x
   ..x.inv <- NULL

   # set a new matrix (assign only if different from existing one)
   # reset the inverse of the matrix
   set <- function( x = matrix() ) {
      assertSquareMatrix( x )
      if( equalMatrix( x, ..x ) ) return()
      ..x <<- x
      ..x.inv <<- NULL
   }

   # set a new inverse matrix (assign only if different from existing one)
   # reset the matrix
   setInv <- function( x.inv = matrix() ) {
      assertSquareMatrix( x.inv )
      if( equalMatrix( x.inv, ..x.inv ) ) return()
      ..x <<- NULL
      ..x.inv <<- x.inv
   }
  
   # return a cached matrix or cumpute it (if only the inverse exists)
   get <- function() {
      if( is.null( ..x ) ) ..x <<- solve( ..x.inv )
      else if( send.message ) message( "getting cached data (..x)" )
      return( ..x )
   }

   # return a cached inverse matrix or cumpute it (if not yet computed)
   getInv <- function() {
      if( is.null( ..x.inv ) ) ..x.inv <<- solve( ..x )
      else if( send.message ) message( "getting cached data (..x.inv)" )
      return( ..x.inv )
   }

   # the list of function
   # (similar to methods of a class in an OO paradigm)
   list( set = set,
         get = get,
         setInv = setInv,
         getInv = getInv )
}



##________________________________________________________________________
## Return the inverse of a cached matrix
## - the argument 'x' is a 'makeCacheMatrix' object
## Note:
## - the inverse of the matrix is computed in 'makeCacheMatrix' environment
## - here, we avoid matrix copies (between different environments)
##
## The inverse is computed only once:
## - this resembles a singleton" pattern
## - such "singleton" is implemented within 'makeCacheMatrix'
cacheSolve <- function( x ) {
   ## return a matrix that is the inverse of 'x'
   return( x$getInv() )
}



##________________________________________________________________________
## Utility functions
##________________________________________________________________________

## Return TRUE if the both matrix, 'x', and 'y', are equal
## - only compares the dimensions and the values
equalMatrix <- function( x, y ){
   equal.matrix <- is.matrix( x ) && is.matrix( y ) && 
                   dim( x ) == dim( y ) && 
                   all( x == y )
   return( equal.matrix )
}


## Assert that the argument ('x') is a square matrix
## - stops excution (with a message) when 'x' is not a square matrix
assertSquareMatrix <- function( x ) {
   # assert that 'x' is a matrix
   assertClass( x, "matrix" )
   # assert that 'x' is square
   dim.x <- dim( x )
   is.square <- all( dim.x == dim.x[ 1 ] )
   if( is.square ) return()

   # stop execution with message that includes the name of the caller function
   msg.prefix <- "!!expected: square matrix"
   msg.in <- " in: "
   caller <- sys.call( sys.parent() )[1]
   msg <- paste( msg.prefix, msg.in, caller, collapse="", sep="" )
   stop( msg )
}


## Assert that the argument ('variable') class is 'class.expected'
## - stops excution (with a message) when 'variable' is not of class 'class.expected'
assertClass <- function( variable, class.expected ) {
   # assert that the class of 'variable' is the expected one
   if( class( variable ) == class.expected ) return()

   # stop execution with message that includes the name of the caller function
   msg.prefix <- "!!expected: class '"
   msg.in <- "' in '"
   caller <- sys.call( sys.parent() )[1]
   msg <- paste( msg.prefix, class.expected, msg.in, caller, collapse="", sep="" )
   stop( msg )
}



##________________________________________________________________________
## a function with test cases
##________________________________________________________________________
test <- function() {
   m   <- matrix( c(4, 7, 2, 6), nrow=2, ncol=2, byrow=TRUE )
   mEq <- matrix( c(4, 7, 2, 6), nrow=2, ncol=2, byrow=TRUE )
   m <- makeCacheMatrix( m, send.message = TRUE )
   print( m )
   cat( "\n" )
   print( m$get() )
   cat( "\n" )
   print( m$getInv() )
   cat( "\n" )
   print( m$getInv() )
   cat( "\n" )
   result <- m$set( mEq ) 
   if( is.null( result ) ) print( "both matrix were equal so nothing was done!" )
   else print( "new matrix was assigned!" )

   cat( "\n" )
   result <- m$setInv( matrix( c(4, 7, 2, 6), nrow=2, ncol=2, byrow=TRUE ) )
   if( is.null( result ) ) print( "both matrix were equal so nothing was done!" )
   else print( "new matrix was assigned!" )
   cat( "\n" )
   print( m$getInv() )
   cat( "\n" )
   print( m$get() )
   cat( "\n" )
   result <- m$set( matrix( c(1, 2, 3, 4), nrow=2, ncol=2, byrow=TRUE ) )
   if( is.null( result ) ) print( "both matrix were equal so nothing was done!" )
   else print( "new matrix was assigned!" )

   cat( "\n" )
   m_inv <- cacheSolve( m )
   print( m_inv )

   cat( "\n" )
   m1 <- makeCacheMatrix()
   #print( m1 )
   cat( "\n" )
   print( m1$get() )
   cat( "\n" )
   print( m1$getInv() )
   cat( "\n" )
   print( cacheSolve( m1 ) )

   cat( "\n" )
   result <- m$setInv( NULL )
   if( is.null( result ) ) print( "both matrix were equal so nothing was done!" )
   else print( "new matrix was assigned!" )
   cat( "\n" )
   print( m$getInv() )
   cat( "\n" )
   print( m$get() )
}



##________________________________________________________________________
## the "entry-point"
test()
# rm( list=ls() )

