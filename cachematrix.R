## This script contains two functions, 'MakeCacheMatrix' and 'cacheSolve' that
## invert a matrix and caches it for later reuse.
## For this script to work, the input matrix needs to be invertible

##sample output:
## > m <- matrix(1:4, 2, 2) # define a matrix
## > m
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > x <- makeCacheMatrix(m) #create a list of functions around matrix m
## > cacheSolve(x) ##first time run, matrix will get inverted
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
# > cacheSolve(x) ##second time run - inverted matrix will be retrived from cache
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


## Write a short comment describing this function
## makeCacheMatrix is a function that takes a matrix and returns a list of functions
## including:
## - get() - which returns the input matrix
## - set() - which caches an input matrix
## - getInverse() - which looks to see if an inverse of the matrix has been cached

makeCacheMatrix <- function(x = matrix()) {
  ci <- NULL
  get <- function() x
  set <- function(x) ci <<- x
  getInverse <- function() ci
  list(get=get, getInverse=getInverse, set=set)
}


## cacheSolve is a function which takes an instance of the makeCacheMatrix function
## which has been run on a given matrix. It checks if the inverse has already 
## been calculated and returns the cached inverse if available. If a cached inverse
## is not available, it will calculate one and then cache this for future use.

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
