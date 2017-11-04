# makeCacheMatrix takes a matrix as input and returns a list of functions that get/set the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


# cacheSolve takes the list returned by makeCacheMatrix and returns the matrix's inverse using the cache if it has been calculated

cacheSolve <- function(x) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    inv <- solve(x$get())
    x$setinv(inv)
    inv
}
