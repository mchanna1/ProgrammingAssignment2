
## Below r functions which are used to cache the matrix and its 
## inverse. During the call to makeCacheMatrix the matrix data is 
## cached. If a attempt is made to get the inverse using cache it 
## returns null. The second function cacheSolve is used to get 
## the inverse from the cached matrix. If inverse is available in 
## cache then it returns the data from cache else it computes the 
## inverse using solve method, caches it and then returns the 
## inverse data.

## Testing - 
## source("cachematrix.R")
## make 2 by 2 
## matrix mt <- matrix(1:4, 2,2)
## cache <- makeCacheMatrix(mt)
## cacheSolve(cache)
## First time it will compute the inverse and return the data
## subsequent calls will return the data from cache.
## Repeat with different matrix


## Below is a function which takes a matrix as a input and caches 
## its value in the environment using << operator. This function 
## in turn has 4 functions set, get, setInverse and getInverse 
## which can be used to set the matrix data, get the matrix data, 
## set the inverse of matrix and get the inverse of matrix 
## respectively.

makeCacheMatrix <- function(x = matrix()) {
	##Initialize inverse to null
	inv <- NULL

	## function to set the matrix data in the cache
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	## function to get the matrix data from cache
	get <- function() x

	## function to set the inverse of a given matrix 
	setInverse <- function(inverse) inv <<- inverse

	## function to get the inverse of a given matrix from cache 
	## (if defined) else returns NULL
	getInverse <- function() inv

	## list object to store the matrix and its inverse
	list(set=set, get=get, 				setInverse=setInverse,getInverse=getInverse)
}


## Below function checks if the inverse of cached matrix is 
## available in the cache then return message with the data else 
## it computes the inverse of matrix, caches it and returns the 
## same.


cacheSolve <- function(x, ...) {
     ## check for inverse
	inv <- x$getInverse()
	## if data is available in cache then return from cache
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	## Compute the inverse using solve method
	inv <- solve(data)
	## set the inverse of matrix in cache
	x$setInverse(inv)
	## return the inverse of matrix
	inv
}

