## Put comments here that give an overall description of what your
## functions do

## This function creates a cache object that can be used to 
## store the matrix and the inverse of the matrix.  

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(x_inv) inv <<- x_inv
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function takes a cache object created with
## the function makeCacheMatrix. It returns the inverse of the 
## source matrix, using the cached value if it has been set and
## setting it if not.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_x = x$getInverse()
  if(is.null(inverse_x)) {
    print("recalc!")
    inverse_x <- solve(x$get())
    x$setInverse(inverse_x)
  }
  inverse_x
}
