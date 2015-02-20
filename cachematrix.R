## Put comments here that give an overall description of what your
## functions do

## Functions to create an special matrix that can calculate and cache inverse matrix

## Write a short comment describing this function

## Object constructor, "x" accepts either a matrix or create an empty matrix as the original matrix
makeCacheMatrix <- function(x = matrix()) {

	m <- NULL					## object to handle inverse matrix
	set <- function(y) {
			x <<- y				## assign matrix passed "y" to original matrix "x""
			m <<- NULL			## reset inverse matrix "m"
	}
	get <- function() x				## function to return original matrix
	setinverse <- function(t) m <<- t		## function to assign inverse matrix
	getinverse <- function() m			## function to return inverse matrix
	list(set = set, get = get,			## main function returns a list with functions
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Write a short comment describing this function

## Return a matrix that is the inverse of the matrix wrapped in object makeCacheMatrix "x"
cacheSolve <- function(x, ...) {

    m <- x$getinverse()					## assign the inverse matrix to "m"
    if(!is.null(m)) {					## if inverse was calculated before (cached)
        message("getting cached data")			## then display a message "getting cached data"
        return(m)					## leave the function and return the cached inverse matrix
    }
	## come here only if inverse matrix was not cached
    data <- x$get()					## get the original matrix
    m <- t(data, ...)					## compute inverse matrix
    x$setinverse(m)					## assign inverse matrix
    m							## Return a matrix that is the inverse of 'x'
}
