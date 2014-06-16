## 1. makeCacheMatrix: This function creates a special "matrix" object 
##    that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. If the inverse has already been calculated 
##    (and the matrix has not changed), then the cachesolve should retrieve the inverse
##    from the cache.

## Create and return special object for inverse matrix caching.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # Sets matrix
  set <- function(y) {
    # Set matrix
    x <<- y
    # Remove cache
    m <<- NULL
  }
  # Returns matrix
  get <- function() x
  # Sets cache
  setInverse <- function(i) inverse <<- i
  # Returns from cache
  getInverse <- function() inverse
  # Returns matrix object
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return cached "solve" for matrix, if no cache then inverse matrix and store it in the cache.
cacheSolve <- function(x, ...) {
  # Get inverse matrix
  inverse <- x$getInverse()
  # Return inverse matrix if cached 
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  # Solve matrix
  i <- solve(data, ...)
  # Cache inverse matrix
  x$setInverse(i)
  i
}
