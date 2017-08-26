## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## make given matrix as a cached one, using closure and <<- operator
## this cache object contains getter/setter for a matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y) {
        x <<- y
        ix <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) ix <<- inv
    getinverse <- function() ix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## calculate the inverse of "cacheMatrix" only once for the same object
##   NOTE: I do not use try/catch, assuming given 'x' is always invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ix <- x$getinverse()
    if(!is.null(ix)) {
        message("getting cached inverse matrix")
        return(ix)
    }
    data <- x$get()
    ix <- solve(data, ...)
    x$setinverse(ix)
    ix
}
