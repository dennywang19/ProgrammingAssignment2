## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function() m <<- solve(x)
    getInverse <- function() m
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve computes the inverse of the special "matrix" returned by the function makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        return(m)
    }
    m <- solve(x$get())
    x$setInverse(m)
    return(m)
}
