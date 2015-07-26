## The first function gets an invertible matrix and makes an
## environment with the matrix and its inverse

## mm is a square invertible matrix

makeCacheMatrix <- function(mm = matrix()) {
	inv <- NULL
	get <- function() mm
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(get=get, #set=set, 
	    setinv=setinv, getinv=getinv)
}


##################################################
## "mcs" = matrix cache solve
##  gets the inverse of a matrix if exists or calculates it

cacheSolve <- function(mcs, ...) {
	inv <- mcs$getinv()
	if (!is.null(inv)) {
		message("getting cached inverse")
		return(inv)
	}
	inversed <- mcs$get()
	inv <- solve(inversed, ...)
	mcs$setinv(inv)
	inv
}

