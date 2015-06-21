## makeCacheMatrix(x = matrix()) function create a list object
## that contains special functions
##1. set() - to set the value of matrix
##2. get() - to get the value of matrix
##3. setinverse() - to set the value of inverse matrix
##4. getinverse() - to get the value of inverse matix
##
## cacheSolve(x, ...) function return inverse matrix of x, where x is 
## a special object creted by makeCacheMatrix()
## in case when inverse matrix is already calculated function returns 
## cached value of inverse matrix otherwise it calculates inverse
## matrix with help of solve() function 

## makeCacheMatrix(x = matrix()) create special list to set and get
## matrix and its inverse matrix
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


## cacheSolve (x, ...) Return inverse matrix, function calculates
## inverse by solve() call or returns cached in x value

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

