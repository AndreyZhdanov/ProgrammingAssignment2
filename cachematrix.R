## makeCacheMatrix(x = matrix()) function creates a list
## that contains special functions
##1. set() - to set the value of the matrix
##2. get() - to get the value of the matrix
##3. setinverse() - to set the value of the inverse matrix
##4. getinverse() - to get the value of the inverse matrix
##
## cacheSolve(x, ...) function returns inverse matrix of x, where x is
## a special object creted by the makeCacheMatrix()
## in case when the inverse matrix is already calculated
## function returns cached value of the inverse matrix otherwise
## calculates the inverse matrix with the help of solve() function 

## makeCacheMatrix(x = matrix()) creates the special list
## to set and getthe matrix and its the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function (y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(solution) inverse <<- solution
	getinverse <- function() inverse
	list (set = set, get = get, setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve (x, ...) returns the inverse matrix
## function calculates inverse by call solve() or returns
## cached in x value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached value")
		return (inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse (inverse)
	inverse
}

# vim: ts=8

