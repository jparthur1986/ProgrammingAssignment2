## These functions allow me to cache the inverse of a matrix and then 
## to return the inverse of that matrix quickly and efficiently.

## This function creates the "makeCacheMatrix" function, which is able to cache the inverse of itself.

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
			x <<- y
			inv <<- NULL
		}
	get <- function() x
	setInverse <- function(solveMatrix) inv <<- solveMatrix
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## "cacheSolve" computes the inverse of the matrix returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}




