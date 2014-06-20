## A caching facility for storing and retrieving the results of solving a matrix.

## Create a caching wrapper for a matrix.
## arguments
## x: a matrix
##
## returns an object with the following properties
## get: returns the value this cache was created with
## set: set a new value for the cache
## getinverse: returns the inverse of the current matrix
## setinverse: set a new value for the inverse of the current matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Create a caching wrapper for a matrix.
## arguments
## x: a matrix cache
##
## returns
## Solves the matrix by first checking if the cached value is set and returning that
## Otherwise, calculate the inverse, store it in the cache, and return the result.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()

  if(is.null(i)) {
    i <- solve(x$get(), ...)
    x$setinverse(i)
  }

  i
}
