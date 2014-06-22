## Compute the inverse of square matrix using cache.

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}

	get <- function() x
	setinversematrix <- function(inversematrix) m <<- inversematrix
	getinversematrix <- function() m
	list(set = set, get = get, setinversematrix = setinversematrix, getinversematrix = getinversematrix)


}




## Computes the inverse of the special "matrix".

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

	m <- x$getinversematrix()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinversematrix(m)
	m

}
