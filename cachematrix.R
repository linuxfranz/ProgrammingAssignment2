## This set of function allows the creation of a special kind of matrix
## and the caching of the inverse of it to provide fast recalculations.

## Creates a special matrix that can keep a cache of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    get <-  function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Computes the inverse on a special matrix object created with
## makeCacheMatrix. If inverse is already computed returns the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix)
    x$setinv(i)
    i
}
