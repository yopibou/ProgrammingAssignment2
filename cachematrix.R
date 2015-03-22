## makeCacheMatrix & cacheSolve functions to create a cache matrix object that can be used repeatedly to get the inverse of the matrix, but only doing the computation once.

## Create Matrix custom object which the inverse is cacheable

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Cumputing the inverse of the custom matrix object, after having checked is the cumputation result was not already available.
##   In that case, previous cached result is returned, no computation is made

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
