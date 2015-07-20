## This function creates a cache matrix which is able to hold the inverse of a square matrix. Since calculating this is potentially a costly operation in terms of computing resources, it is beneficial to cache the computation.
## It serves as a cache for that calculation.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set=set, get=get, 
       setmatrix=setmatrix, 
       getmatrix=getmatrix)
}

## This function checks if the inverse of the matrix, which is stored in the cache matrix, has already been computed.
## If so, the cached inversed matrix is retrieved.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
          message("getting cached data")
          return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}