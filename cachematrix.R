## These functions take a matrix as input,
## calculate, chache and return the inverse of the matrix
## and if it is already calculated return the cached value.

## makeCacheMatrix creates a "vector", which contains functions to
## set the value of the vector
## get the value of the vector
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve checks if inverse is already calculated, if it is, it returns the chached value
## otherwise calculates the inverse, chache the result and returns it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
