## This function "makeCacheMatrix" generates a special "matrix" object that can cache its inverse.
## setting matrix value
## getting matrix value
## setting inverse value
## getting inverse value

makeCacheMatrix <- function(x = matrix()) {
## inverse matrix value
inverse <- NULL
## set matrix value
set <- function(y) {
  x <<- y
  inverse <<- NULL
}
## get matrix value
get <- function() x
## set inverse value
setinv <- function(invdata) inverse <<- invdata
## get inverse value
getinv <- function() inverse
## return a list of all  functions
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}

## This is to calculate the special "matrix" inverse returned by makeCacheMatrix above.
## It has to validate if the inverse was calculated 
## or the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## validate that the inverse is cached,
  ## obtain the inverse from cache
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## we get the matrix first
  data <- x$get()
  ## inverse is calculated
  inv <- solve(data, ...)
  ## cache the matrix inverse
  x$setinv(inverse)
  ## the result is returned
  inverse
}
