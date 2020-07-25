#This function takes a matrix and defines the get, set 
#functions for the matrix and its inverse.
#x is the inputted matrix and xinv is its inverse
makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
	set <- function(y){
		x <<- y
		xinv <<- NULL 
	}
	get <- function() x
	setinv <- function(inv) xinv <<- inv
	getinv <- function() xinv
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)
}
#This function calculates the inverses of the
#matrix if and only if we don't already have it cached
#so that it can save computation time!
#I've used the solve() function for computating the
#inverse of the matrix x.
cacheSolve <- function(x, ...) {
	xinv <- x$getinv()
	if(!is.null(xinv)) {
		message("getting cached data")
		return(xinv)
	}
	data <- x$get()
	xinv <- solve(data, ...)
	x$setinv(xinv)
	xinv
}
