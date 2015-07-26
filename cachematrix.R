## These functions will create an object that stores a matrix and
## caches its inverse

## This function will get and set the values of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvs <- function(solve) m <<- solve
  getinvs <- function() m
  list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)
}


## This calculates the inverse of the matrix created. First it
## checks to see if the inverse was calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise it calculates the
## inverse of the data and sets the value of the inverse in the cache

cacheSolve <- function(x, ...) {
  m <- x$getinvs()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvs(m)
  m 
  ## Return a matrix that is the inverse of 'x'
}

