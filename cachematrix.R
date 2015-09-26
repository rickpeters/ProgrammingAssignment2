#
# create a special matrix that has functions for getting and setting the matrix
# as well as functions for setting and getting the inverse of the stored matrix
# this resembles something of a matrix class with embedded functions
# another function uses this class to calculate the inverse of the matrix
# and stores the result so in a consecutive call the inverse can be retrieved
# from the cache instead of calculAted again
#
# USAGE EXAMPLE:
# > x <- makeCacheMatrix(matrix(2,2, 1:4))
# > y <- cacheSolve(x)
# > y
# > y
#

# makeCacheMatrix creates a special matrix with get and set functions, and 
# functions for getinverse and setinverse for the inverse value of the matrix
# as calculated by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve calculates the inverse of the matrix in the special matrix class
# which is created by makeCacheMatrix. the inverse is stored inside the matrix
# object so it can be read from cache on the next call
#
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
