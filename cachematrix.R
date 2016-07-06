## These functions allow the inverse of a matrix to be cached, which is 
## desirable because it can be a time-consuming operation. These functions
## assume that the input matrix x is invertible.

## makeCacheMatrix creates a list of functions, which can be passed along
## with the matrix object x to cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv, getinv = getinv)
}


## cacheSolve computes and returns the inverse of matrix x if the inverse
## is not already in the cache; if so, it returns the cached inverse.

cacheSolve <- function(x, ...) {
inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv	
}
