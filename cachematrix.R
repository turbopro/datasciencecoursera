### R Programming - Programming Assignment 2

### Date: 04-21-2014
### By:   Vern
### This code based on the 'makeVector' and 'cachemean' fucntions
### Branched from: https://github.com/rdpeng/ProgrammingAssignment2

## The 'makeCacheMatrix' and 'cacheSolve' functions are used
## to: 
##   1) create a special object that stores a matrix (invertible) to
##      allow for caching of the computed inverse of the matrix, and
##   2) cache the computed inverse of the matrix
##
## Given that computing the inverse of a matrix may be computationally
## expensive, the above two functions allow for the caching of the 
## result for future use.  If the matrix does not change, then the
## inverted matrix may be retrieved from the cached result

## NB: we assume the matrix is always invertible

#  makeCacheMatrix:
#  Creates the special matrix object that is a list containing
#  the following four functions:
#    set the value of the matrix
#    get the value of the matrix
#    set the value of the inverted (solved) matrix
#    get the value of the inverted (solved) matrix

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

#  cacheSolve:
#  Computes, caches, and returns the inverse of the special
#  matrix created by 'makeCacheMatrix'
#  If the inverse already has been calculated (and the matrix
#  has not changed), then the fucntion returns the cached
#  inverted matrix

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
