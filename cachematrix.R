## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ci <- NULL
  get <- function() x
  set <- function(x) ci <<- x
  getInverse <- function() ci
  list(get=get, getInverse=getInverse, set=set)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverted <- x$getInverse()
  
  if(!is.null(inverted)) {
    message("getting cached data")
    return(inverted)
  }
  
  mx <- x$get()
  inverted <- solve(mx)
  x$set(inverted)
  inverted
}
