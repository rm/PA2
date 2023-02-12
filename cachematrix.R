## Solution to Programming Assignment 2

## This function creates a special "matrix" object 
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  
  set <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(inverse) 
    cached_inverse <<- inverse
  
  get_inverse <- function() cached_inverse
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$set_inverse(inverse)
  inverse
}
