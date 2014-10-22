## The following a pair of functions can cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Invm <- NULL
  set <- function(y) {
    x <<- y
    Invm <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) Invm <<- inv
  getInverse <- function() Invm
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  Invm <- x$getInverse()
  if(!is.null(Invm)) {
    message("getting cached data")
    return(Invm)
  }
  data <- x$get()
  Invm <- solve(data, ...)
  x$setInverse(Invm)
  Invm
}
