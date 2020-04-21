## Function to cache an inverse Matrix

## This function creates an environment where the inverse matrix is cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}

## This function uses the solve function to create the inverse matrix
## which is then sent to the cache

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) { 
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
