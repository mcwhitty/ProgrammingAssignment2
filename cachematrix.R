## Author: Marc C Whittaker
## Course: R Programming - Coursera
## Assignment Week 3

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(mymatrix = matrix()) {
  myinverse <- NULL
  setMatrix <- function(y) {
    mymatrix <<- y
    myinverse <<- NULL
  }
  getMatrix <- function() mymatrix
  setInverse <- function(inverse) myinverse <<- inverse
  getInverse <- function() myinverse
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache
cacheSolve <- function(x, ...) {
  myinverse <- x$getInverse()
  if (!is.null(myinverse)) {
    message("Returning cached inverse matrix")
    return(myinverse)
  }
  mat <- x$getMatrix()
  myinverse <- solve(mat, ...)
  x$setInverse(myinverse)
  myinverse
}
