## Put comments here that give an overall description of what your
## functions do

## Create a "special" matrix object that allows the inverse to be stored
## along with the methods to access & edit both the matrix & its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(invValue) inv <<- invValue
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function searches for a cached copy of matrix inverse, before comitting to solving the matrix

cacheSolve <- function(x, ...) {
    invValue <- x$getInverse()
    
    if (is.null(invValue)) {
        inv <- solve(x$get(), ...)
        x$setInverse(inv)
        inv
    } else {
        message("Getting cached inverse...")
        return(invValue)
    }
}
