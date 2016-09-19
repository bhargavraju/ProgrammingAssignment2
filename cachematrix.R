## The following functions calculate inverse of a matrix and saves it
## to the cache such that the next time the user attempts to calculate the
## same matrix inverse, the saved cache value is returned without
## repeating the calculation.

## This function creates a matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  #define cache
  inv <- NULL
  set <- function(y) {
    ##assign the input matrix y to the variable x in the parent environment
    x <<- y
    ##reinitiate inv to null
    inv <<- NULL
  }
  get <- function() x
  ## set the cache inv equal to the inverse of the matrix x
  setInverse <- function(inverse) inv <<- inverse
  ## return the cached inverse of x
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse of matrix. It checks if the inverse is already in cache.
## If it is then it gets it. If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

