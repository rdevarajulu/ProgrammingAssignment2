## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function creates a matrix
## encapsulated by a list with associated setters/getters.
## One set/get to get to the raw matrix data.
## One set/get to get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	set <- function(y) {
		x <<- y
	}
	get <- function() x
	
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	
	list(set = set, get = get
	, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

## The cacheSolve function returns the inverse of a matrix
## created with the makeCacheMatrix function above.
## The function either calculates the inverse and stores it
## in the cache, or returns the inverse from the cache

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	
	if (!is.null(i)) {
		message("getting cached inverse")
		return(i)
	}
	
	mat <- x$get()
	i <- solve(mat, ...)
	x$setinverse(i)
	
	## Return a matrix that is the inverse of 'x'
	i
}