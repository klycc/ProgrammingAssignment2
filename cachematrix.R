## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    	inverse <- NULL
    	set <- function(y) {
        	x <<- y
		inverse <<- NULL
    	}
    	get <- function() x
    	setinverse <- function(inv) {
      		inverse <<- inv
    	}
    	getinverse <- function() inverse 

    	list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
## Here we assume the input matrix is always invertible.
cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()
	if (!is.null(inverse)) {
	        message("getting cached inverse matrix")
		return(inverse)
	}
	data <- x$get()
	## get the inverse of matrix data
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
