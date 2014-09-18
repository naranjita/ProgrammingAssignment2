## R Programming, Assignment 2
## This .R file contains a pair of functions as specified below:

## makeCacheMatrix: Creates a "matrix" object that can cache its inverse.

## cacheSolve: Computes the inverse of the "matrix" returned by makeCacheMatrix
## but if the inverse has already been calculated and the "matrix" has not 
## changed, then the cachesolve should retrieve the inverse from the cache.


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## makeCacheMatrix: Creates a "matrix" object that can cache its inverse.  ##
## INPUT: the matrix for which the inverse has to be calculated.           ##
## OUTPUT: the special "matrix" object that can cache its inverse.         ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

makeCacheMatrix <- function(x = matrix()) {
        ## matrix initialization
        m <- NULL
        ## set the value of the matrix
        set <- function(matrix) {
                x <<- matrix
                m <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse matrix
        setInv <- function(inv) m <<- inv
        ## get the value of the inverse matrix
        getInv <- function() m
        ## return a list with the methods
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## cacheSolve: Computes the inverse of the "matrix" returned above.        ##
## INPUT: special "matrix" for which the inverse is to be calculated.      ##
## OUTPUT: inverse of the special "matrix".                                ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        ## check if the inverse matrix has been already cached
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## get matrix
        data <- x$get()
        ## calculate inverse using solve
        m <- solve(data, ...)
        ## set the inverse of the matrix
        x$seInv(m)
        ## return the matrix
        m
}
