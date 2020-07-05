## The function is used to create a matrix that we can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <- y
    inv <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <- inverse
  getinverse <- function() inv
}  

## This function is used to return the inverse of the matrix "x" 
## if the inverse was calculated earlier the cacheSolve will retrieve it from the cache


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
