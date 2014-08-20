## These are pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse. Result 
## of the function is a list of functions 

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
	  set <- function(y) {
	  ## x and m are both stored in a different environment
    	x <<- y
    	m <<- NULL
	  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  ## creating list of functions	  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve is retrieving the inverse from the cache.

cacheSolve <- function(x, ...) {
## Getting cashed data
  m <- x$getsolve()
## If cached data exist, return its value and stop the function 
  if(!is.null(m)) {
		message("getting cached data")
    	return(m)
  	}
## Else get the matrix, calculate its invert, assign it to m variable activated 
## from another environment, and output its value
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
