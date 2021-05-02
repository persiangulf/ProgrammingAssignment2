## Functions to cache matrix inversion.
##
## The functions in this module provide a convenient interface
## to access and cache the inverse of a matrix.  Example usage:
##
##   # Create an object to store the matrix and cache its inverse
##   cacheableMatrix = makeCacheMatrix( rbind(c(10, 20), c(30, 40)) )
##
##   # The first call to cacheSolve will calculate the inverse,
##   inverse1 = cacheSolve(cacheableMatrix)
##
##   # Further calls to cacheSolve will be significantly faster, since
##   # they return the value cached during the previous call.
##   inverse2 = cacheSolve(cacheableMatrix)


## makeCacheMatrix -- creates an object to hold a matrix and (eventually)
## its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  cachedinverse <- NULL
  set <- function (newx) {
    x <<- newx
    cachedinverse <<- NULL
  }
  get <- function() x
  setcachedinverse <- function(inverse) cachedinverse <<- inverse
  getcachedinverse <- function() cachedinverse
  
  list(set = set, get = get, setcachedinverse = setcachedinverse, getcachedinverse = getcachedinverse)
}


## cacheSolve -- returns the inverse of the matrix stored in a
## 'makeCacheMatrix' structure.  Further calls to cacheSolve() for
## the same 'makeCacheMatrix' structure will return a cached inverse.

cacheSolve <- function(x, ...) {
  cached <- x$getcachedinverse()
  if (! is.null(cached)) {
    return(cached)
  }
  m <- x$get()
  cached <- solve(m)
  x$setcachedinverse(cached)
  cached
}
