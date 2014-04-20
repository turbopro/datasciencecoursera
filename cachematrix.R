## The 'makeCacheMatrix' and 'cacheSolve' functions are used
## to: 
##   1) create a special object that stores a matrix (invertible)
##      to allow for caching of the inverse of the matrix
##   2) cache the inverse of the matrix
##
## If the matrix does not change, then the the cached inverse
## becomes availble for use later without the need to compute
## the inverse afresh

## Note: we assume the matrix is always invertible

## makeCacheMatrix:
## Creates the special matrix object that is a list containing
## the following four functions:
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the reversed (solved) matrix
##   get the value of the reversed (solved) matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get, setsolve = setsolve,
          getsolve = getsolve)
}

## cacheSolve:
## Computes the inverse of the returned special matrix, which
## contains the list of four functions 
## If the inverse already has been calculated (and the matrix
## has not changed), then the fucntion retrieves the inverse
## from the cache

cacheSolve <- function(x, ...) {
     m <- x$getsolve()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
}
