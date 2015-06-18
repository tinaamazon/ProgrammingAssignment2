## Two functions to cache a matrix, compute and cache its inverse

## 1. Create a special "matrix" that can cache its inverse
##    This "matrix" is actually a list of four functions:
##    set the value of a matrix/ get the value of a matrix/ 
##    set the value of the inverse matrix/ get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y) {
        x <<- y
        ## invalidate old inverse
        ix <<- NULL     
    }
    get <- function() x
    setinv <- function(inv) ix <<- inv
    getinv <- function() ix
    ## Return the four functions in a list
    list(set = set, get = get, setinv = setinv, getinv = getinv)
} 

## 2. Compute the inverse of the special matrix returned by makeCacheMatrix.
##    If the inverse has been already calculated (and matrix is unchanged),
##    take the inverse from the cache. Otherwise compute the inverse anew.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## First check if the inverse has been already computed
    ix <- x$getinv()
    if(!is.null(ix)){
        message("getting cached data")
    }
    data <- x$get()
    ## Compute the inverse matrix
    ix <- solve(data, ...)
    ## Cache the inverse matrix
    x$setinv(ix)
    ix
}
