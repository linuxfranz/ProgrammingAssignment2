## This set of functions allows the creation of a special kind of matrix
## that caches the inverse of itself to provide fast recalculations.

## Function makeCacheMatrix creates a special matrix that can keep a 
## cache of its inverse. Argument can be a regular matrix. If called 
## without an argument creates an empty matrix.
## The resulting list obj offers the following functions:
## obj$get() returns the matrix
## obj$set(matrix) sets the matrix
## obj$getinv() returns the cached inverse matrix calculating it if necessary
## obj$setinv(matrix) sets the inverse matrix

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


## Function cacheSolve computes the inverse on a special matrix 
## object created with makeCacheMatrix. If inverse is already computed 
## returns the cache. 
## Argument is a cached matrix object created with makeCacheMatrix.

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
