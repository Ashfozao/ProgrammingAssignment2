## this function computes the inverse of the 
## special “matrix” returned by makeCacheMatrix above


makeCacheMatrix <- function(x = matrix()) {
         i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
}


## It retrieves the inverse from the cache if 
## the inverse has already been calculated

cacheSolve <- function(x, ...) { 
        i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
