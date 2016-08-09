## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Following function takes in a matrix as parameter and initializes an R object and sets its value in the parent environment
## It also returns a list of named methods for easy access in the local environment, while the methods returns values stored in the Parent environment
## set - Method that sets value x in the Parent environment
## get - Method that returns the value of x stored in the parent environment
## setinverse - Method that sets the value of inverse 
## getinverse - Method that gets the value of inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, 
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Write a short comment describing this function
## Following method uses solve to find the inverse of the matrix if the value is not already available in the Cache (parent environment)
## Output of makeCacheMatrix should be passed to this to retrieve the value
## e.g. xM <- makeCacheMatrix(matrix(rnorm(1:4),nrow=2,ncol=2))
##      cacheSolve(xM) should output the Inverse of xM
## cached value can be reset whenever you set a different value for xM using xM$set(newValue)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if (!is.null(m)) {
		message("Getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m		
}
