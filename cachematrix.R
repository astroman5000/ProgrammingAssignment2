## The functions in this file are used to calculate and cache the inverse
## of a matrix.

## The makeCacheMatrix function is used to create a cached inverse matrix.
## This function returns a special vector that provides functions for getting
## and setting the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function wil try to find the inverse matrix in the cache and return it.
## If the inverse matrix is not found, it will calculate the inverse matrix by using the solve function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
