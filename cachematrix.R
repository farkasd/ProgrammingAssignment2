## In this assignment I will cache the inverse of a matrix.

## Reason of caching the inverse of a matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly. 
## I will write a pair of functions that cache the inverse of a matrix.


## This makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## inv is the cache for the matrix inverse. It is set to NULL.
        set <- function(y) {  ## subfunction, assigns a matrix to the variable x and clears the cache.
                x <<- y
                inv <<- NULL
        }
        get <- function() x ## subfunction, returns the matrix which is stored in x.
        setinverse <- function(solve) inv <<- solve ## subfunction, takes the matrix passed into it and stores 
                                                        ##in the catche.
        getinverse <- function() inv ## subfunction, returns the inverse
        list(set = set, 
                get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() ## return the inverse from cache.
        if(!is.null(inv)) { ## if the cache is not empty than gives the inverse of the matrix from cache.
                message("getting cached data")
                return(inv)
        }
        ## if the cache is empty, the cachSolve function does the following:
        data <- x$get() ## gets the matrix and place it into a local variable called data.
        inv <- solve(data, ...) ## calculates the inverse of the matrix and puts it into a local variable, inv.
        x$setinverse(inv) ## stores the calculated inverse into the cache.
        inv ## returns the inverse.
}
