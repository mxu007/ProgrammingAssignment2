## makeCacheMatrix creates a special vector that 
## set the value of matrix
## get the value of matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL	# Initialize the inverse i to NULL
	set <- function(y) {
		x <<- y	# when matrix x is set to a new value
		i <<- NULL	# inverse i is initialized to NULL
	}
	get <- function() x	# return matrix x
	setinverse <- function(inverse = matrix()) i <<- inverse	# set inverse
	getinverse <- function() i	# return inverse i

	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
	# a <- makeCacheMatrix(SampleMatrix)
	# a$set to set new value to matrix x
	# a$get to return current matrix x
	# a$setinverse to set the inverse matrix 
	# a$getinverse to return current inverse matrix 
}


## cacheSolve computes the inverse of special matrix
## If the inverse is cached, return the cached value
## If not, compute the inverse of matrix by function solve

cacheSolve <- function(x, ...) {
	i <- x$getinverse()	#attempt to retrieve cached inverse matrix
	if(!is.null(i)){
		message("getting cached inverse result")
		return(i)	#If inverse is cached, return the cached value
	}
	data <- x$get()
	i <- solve(data, ...)	#If not, perform the inverse calculation
	x$setinverse(i)
	i
}
