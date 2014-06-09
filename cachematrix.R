## this two function used to cache inverse value of a matrix

## create special matrix can cache value

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## create cachesolve function, cache inverse of a matrix

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if (!is.null(m)) {
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
