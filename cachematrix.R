## This is the assignment #2 for the R Programming Coursera course
## The first function below caches attributes of a matrix
## The second function retrieves the cached inverse if it exists
## otherwise it computes it directly


## This function defines functions for getting and setting a matrix
## Input: A square matrix object
## Output: A list object that contains functions for getting and setting
## matrix data
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of a matrix. If the inverse 
## has already been calculated, the cached value is returned
## Input: The output of the makeCacheMatrix function
## Output: A matrix object that is the inverse of the input matrix
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
