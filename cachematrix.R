## This file have 2 functions to create a special matrix to cache matrix values
## and verify if the cached value is already cached or not, and show the value

## The function makeCacheMatrix create a special matrix that will keep
## the calculeted value

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
          x <<- y
          s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The cacheSolve function verify if x matrix already have the inverse calculated
## if the inverse is already cached, then the function returns the value
## if not, the function calculates the value and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
          message("getting cached data")
          return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
