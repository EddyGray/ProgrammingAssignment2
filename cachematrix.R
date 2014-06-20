## This file contains functions which return the inverse of a 
## square matrix.
## The inverse matrix will be calculated the first time called,
## and subsequently a cached version of the result will be used.

# Can test with z <- makeCacheMatrix(A)
# followed by cacheSolve(z)

## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setInverseMatrix <- function(invMatrix) m <<- invMatrix
  
  getInverseMatrix <- function() m
  
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}

## Return a matrix that is the inverse of 'x'

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function.
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  invMatrix <- x$getInverseMatrix()
  
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  
  message("calculating inverse matrix")
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setInverseMatrix(invMatrix)
  
  invMatrix
}
