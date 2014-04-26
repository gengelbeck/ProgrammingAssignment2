## Use cachematrix to save time recalculating the inverse of a matrix.
##
## cachematrix contains functions that:
## 1. compute the inverse of a matrix, and
## 2. cache the inverse so it does not need to be recalculated

## Limitations:
## 1. The functions assume that the matrix supplied is always invertible
## 2. There is not a check that the input to cacheSolve is a makeCacheMatrix

## AUTHOR: George Engelbeck
## CREATED: Sat Apr 26 09:52:34 2014
## MODIFIED: Sat Apr 26 13:55:34 2014
## - inital creation

makeCacheMatrix <- function(x = matrix()) {
  # Creates a special matrix that is a list of functions that:
  # 1. sets the value of the matrix
  # 2. gets the value of the matrix
  # 3. sets the inverse of the matrix
  # 4. gets the inverse of the mean
  #
  # If a cached value exists the cached value is returned
  # If a cached value does not exists, a Null is returned
  #
  # Args:
  #   x: a matrix whose inverse is to be calculated.
  #
  # Returns:
  #   The inverse of the matrix x.
  
  #  Private variable
  p_inv <- NULL
  
  #  Set the value of the matrix x
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #  Get the value of the matrix x
  get <- function() x
  
  #  Set the value of the inverse of matrix x
  setinverse <- function(m_inverse) p_inv <<- m_inverse
  
  #  Set the value of the inverse of matrix x
  getinverse <- function() p_inv
  
  #  Return the list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Computes and caches the inverse of a matrix.
  # If a cached value exists the cached value is returned
  # If a cached value does not exists the value is computed, cached, and returned
  #
  # Args:
  #   x: a matrix whose inverse is to be calculated.
  #
  # Returns:
  #   The inverse of the matrix x.
  
  ## PRECONDITIONS:
  ## x is a matrix produced by makeCacheMatrix - this is not checked
  
  ## POSTCONDITIONS
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  
  # If there is a cached inverse of martrix x return it
  if(!is.null(im)) {
    # message("getting cached inverse of matrix")
    return(im)
  }
  
  # If there is not a cached inverse of martrix compute it, cache it, and return it
  data <- x$get()  # get the matrix
  im <- solve(data, ...)  # compute the inverser
  x$setinverse(im) # cache the inverse
  im # return the inverse
  
}
