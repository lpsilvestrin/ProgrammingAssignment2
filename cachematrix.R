## Both functions follow the "cachemean" example

## Stores a square matrix and its inverse. The inverse is initialized
## with NULL until it is initialized either by cacheSolve or
## setInverse.
## The set method reset the cached inverse to NULL.
## getDim method returns the dimension of the square matrix as a
## numeric vector of length 1

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) i <<- inv
	getInverse <- function() i
	getDim <- function() dim(x)[1]
	list(set = set, get = get, setInverse = setInverse,
		getInverse = getInverse, getDim = getDim)
}


## input: cacheMatrix
## output: matrix
## calculate the inverse of a cacheMatrix c and cache it within c
## return the cached inverse if it was already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
        	return(i)
        }
        I <- diag(x$getDim())
        data <- x$get()
        i <- solve(data, I, ...)
        x$setInverse(i)
        i
}	
