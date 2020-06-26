## There are a pair of functions that cache the inverse of a matrix
## They are: makeCacheMatrix & cacheSolve

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  SetInvert <- function(inv) m <<- inv
  GetInvert <- function () m
  list(set = set, get = get,
       SetInvert = SetInvert,
       GetInvert = GetInvert)
}


## cacheSolve - computes the inverse of the special "matrix" returned by the function above. If the
## inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieve the
## inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$GetInvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$SetInvert(m)
  m
}