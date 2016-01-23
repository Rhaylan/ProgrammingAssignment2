## There are four functions created: set, get, setInverse and getInverse
## set changes the value stored in x and get returns the value stored in x 
## setInverse uses the solve function to create the inverse matrix and 
## getInverse returns the value of mn

makeCacheMatrix <- function(x = matrix()) {
	mn <- NULL
	set <- function(y){
		x <<- y
		mn <<- NULL
	}

	get <- function() x
	setInverse <- function(inverse) mn <<- inverse
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse,
	getInverse = getInverse)
}

## Takes the special matrix returned by makeCacheMatrix().  It first checks to see if 
## the inverse matrix has already been calculated.  It if has it returns the already cached value.  If not it 
## creates the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of ‘x’
	mn <- x$getInverse()
	if(!is.null(mn)){
		message("Getting the cached data for mn")
		return(mn)
	}

	myData <- x$get()
	mn <-solve(myData, ...)
	x$setInverse(mn)
	return(mn)
}
