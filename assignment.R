## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(sample(1:300,10),5,5)) {
  k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) k <<- inverse
  getinverse <- function() k
  list(set = set, get = get,
       setsd = setinverse,
       getsd = getinverse)
} 

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will be able to retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachSolve<- function(x, ...) {
    k <- x$getinverse()
    if(!is.null(k)) {
      message("getting inverse data")
      return(k)
    }
    data <- x$get()
    k <- solve(data, ...)
    x$setinverse(k)
    k
  }