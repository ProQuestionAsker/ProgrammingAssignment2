## This script creates functions within a function 
## Together, they allow one to cache a matrix
## and use the cached values to invert that matrix

## This function assigns the cached values for a matrix
## Descriptions of each step are listed after # above line

makeCacheMatrix <- function (x = matrix()){
	# creates a function with an input matrix

	# initializes m as object in makeCacheMatrix environment
	m <- NULL

	# sets the global value of x to be y
	# assigns input argument of x in parent environment
	# assigns value of NULL to m in parent environment
	set <- function(y){
		x <<- y
		m <<- NULL
	}

	# retrieve x from parent environment
	get <- function() x

	# defines the setter for global variable m
	# accesses m after setinverse() completes from parent
	setinverse <- function(solve) m <<- solve

	#defines the getter for the inverse m
	getinverse <- function() m

	# assigns each function as element in list
	# returns list to parent environment
	# also names all functions defined above
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## This function looks for and accesses a cached matrix
## The cached matrix (if found) is then inverted and printed
## Details for each step are found after # above each line

cacheSolve <- function(x, ...){
	# accesses getinverse in x matrix and assigns to m
	m <- x$getinverse()

	# if m exists and isn't NULL, return message and
	# print m
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}

	# if m doesn't exist, assign variable to get data
	data <- x$get()

	# assign variable m to function mean of data above
	m <- solve(data, ...)

	# calls for the setinverse in x matrix
	x$setinverse(m)
	m
}
