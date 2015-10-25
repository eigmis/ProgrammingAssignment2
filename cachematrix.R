## Contains functions to calculate and cache the inversion of the matrix.


## Creates a special "matrix" structure (a list) that allows setting and getting 
## the inverse of the provided matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) i <<- inverse
  get.inverse <- function() i
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## Given the special matrix (created with the makeCacheMatrix function) as a parameter
## returns cached inverted matrix or if the cached result does not exist - calculates
## the inverted matrix, chaces it and returns the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$get.inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set.inverse(i)
  i  
}
