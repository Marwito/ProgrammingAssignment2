# The following functions were defined based on the "Caching the Mean of 
# a Vector" example. The first function "makeCacheMatrix" will create a matrix
# whose inverse will be computed by "cacheSolve". If the inverse has already 
# been calculated and the matrix has not # changed, then cacheSolve retrieves
# the inverse from the cache.

# This function creates a special "matrix" object that can cache its inverse.
# It contains a list of functions to set/get the value of the matrix and to 
# set/get the value of its inverse

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

# This function computes the inverse of the special "matrix" using the 'solve'
# function or prints out the message "getting cached data" and retrieves the
# inverse from the cache.

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
