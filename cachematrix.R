## This file contains two functions that permit a use of previously cached inversed matrices in order
## to save computation resources.

 ## makeCacheMatrix is a function that wraps a matrix into a list containing other useful methods that
 ## are later used by cacheSolve.
 ## parameters:
 ## x: a matrix that later will be inverted, under the assumption that x is invertable. 
makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  get <- function() x
  setCache <- function(cache) c <<- cache
  getCache <- function() c
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}


## cacheSolve detects if a matrix was previously inversted and in case if it was then
## the cached version is returned, otherwise the inverse operation is performed and a freshly
## inversed matrix is returned.
## parameters: 
## x: a list that was returned by makeCacheMatrix function above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  c <- x$getCache()
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$setCache(c)
  c
}
