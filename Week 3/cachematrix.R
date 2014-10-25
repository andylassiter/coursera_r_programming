## Creates a matrix object with get and set methods for the matrix
## and the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv 
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## Creates a method to return a cached inverse or compute the inverse
##  if it has not already been computed
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, diag(nrow(data)))
  x$setInverse(inv)
  inv
}