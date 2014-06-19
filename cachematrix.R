## R Programming
## Project Assignment 2: Caching the Inverse of a Matrix

## Given that matrix inversion is often a costly computation, there are benefits
## to caching the inverse to avoid computing it multiple times.  The pair of
## functions below implements a version of matrix whose inverse is cached once
## it is calculated. 

## makeCacheMatrix creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    getInverse <- function() {
        inv
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of the special "matrix" created by above:
##   1) returns the cached inverse if it exists
##   2) computes the inverse otherwise

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
