## ---------------------------
##
## cachematrix.R
##
## Author: Gary Guilbeaux
## Date: 07/29/2024
##
## Purpose: Cache the inverse of a matrix
##
## Usage:
## > z <- makeCacheMatrix(sqmatrix) # my_matrix must be square
## > inv_sqmatrix <- cacheSolve(z)  # inverts and returns inverted square matrix
## > inv_sqmatrix <- cacheSolve(z)  # reports "getting cached data" and returns 
## getting cached data              # cached inverted square matrix
##                                  


makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object 
  ## that can cache its inverse.
  ##
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## This function computes the inverse a special "matrix" 
  ## returned by makeCacheMatrix. If the inverse has already been 
  ## calculated (and the matrix has not changed), then cacheSolve retrieves
  ## the inverse from the cache.
  ##
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- 1/data
  x$setinv(m)
  m
}
