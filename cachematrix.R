## Functions makeCacheMatrix and casheSolve
## for Programming Assignment 2: Lexical Scoping
## Irek Zawadzki 1/24/16

# This function creates a special "matrix" object
# that can cache its inverse.
# it is actually a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) invMat <<- solve
    getinverse <- function() invMat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# This function returns the inverse of a special matrix created with
# makeCacheMatrix function. It checks if the inverse has already been computed.
# If the inverse was computed it retrieves the result.
# Otherwise it computes the inverse, saves it in a cache and returns the result.
cacheSolve <- function(x, ...) {
    invMat <- x$getinverse()
    if(!is.null(invMat)) {
        message("getting cached data")
        return(invMat)
    }
    data <- x$get()
    invMat <- solve(data, ...)
    x$setinverse(invMat)
    invMat
}
