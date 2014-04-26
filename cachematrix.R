## Two functions, one to store matrices and one to compute
## the inverse of a given matrix


## makeCacheMatrix 	stores a special matrix and the inverse of
## 					it, if it was previously computed already

makeCacheMatrix <- function(x = matrix()) {
	theinverse <- NULL
	# Set the matrix
	set <- function(y) {
			x <<- y
			# making sure the inverse is NULL if a matrix is changed/created, 
			theinverse <<- NULL
	}
	get <- function() x
	# Set a new inverse
	setinverse <- function(inverse) theinverse <<- inverse
	# Get the inverse
	getinverse <- function() theinverse
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## cacheSolve		returns the inverse of a special matrix. It checks first
##					if that was stored before and returns the stored
##					value if available

cacheSolve <- function(x, ...) {
	## Try to get a cached inverse from the object
	theinverse <- x$getinverse()
	## If we got something meaningful = not null, return that
	if(!is.null(theinverse)) {
			message("getting cached data")
			return(theinverse)
	}
	## If we did get null, this block computes the mean, then
	## stores and returns it
	data <- x$get()
	theinverse <- solve(data, ...)
	x$setinverse(theinverse)
	theinverse
}
