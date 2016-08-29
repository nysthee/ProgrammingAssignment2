## This is my solution for Programming Assignment 2: Lexical Scoping.
## We had to write a function that wrapped a matrix with some extra functionality
## and a function that stored/resolved the inverse of a given matrix in cache.

## This function is a builder function to create a matrix with some accessors.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function resolves the inverse of the matrix and returns the cached inverse of the matrix
## when it is already calculated or calculates it when not not in cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
