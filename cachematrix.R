## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss
## here). 
## Functions:
##
## makeCacheMatrix: This function creates a special "matrix" object that can
##                  cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix"
##             object returned by makeCacheMatrix function. If the inverse has
##             already been calculated (and the matrix has not changed), then
##             the cachesolve should retrieve the inverse from the cache.
##
## Use example:
## source("cachematrix.R")
## x <- matrix(c(1,2,3,4), 2, 2)
## y <- makeCacheMatrix(x)
## z <- cacheSolve(y)


## function makeCacheMatrix creates a special "matrix", which is really a list
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. setInv the value of matrix inversion
## 4. getInv the value of matrix inversion
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) inv <<- Inv
        getInv <- function() inv
        list(get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

## function cacheSolve
## if x$getInv() in empty then function calculates the inverse of a matrix
## and store result in cache
## otherwise, returns x$getInv()
cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        return(inv)
}
