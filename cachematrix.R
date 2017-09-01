## These functions are used as a set to solve and store inverse matrices
## A matrix with additional info stored as a list is created with makeCacheMatrix
## When a matrix with additional info is passed to cacheSolve, cacheSolve will 
## Return the inverse of the matrix passed to it.
## If the inverse of a given matrix was previously calculated, cacheSolve will
## return a cached value of the inverse matrix instead of solving for the inverse again

## Given a matrix x, makeCacheMatrix will return a list containing 
##  - set() : a function to change the matrix data
##  - get() : a function that returns the actual matrix data
##  - setInverse() : a function that changes the stored inverse of the matrix
##  - getInverse() : a funciton that returns the stored inverse of the matrix

## If the matrix data is modified with set(), or the inverse has not been stored yet
## getInverse() will return null

makeCacheMatrix <- function(x = matrix()) {
  
  ## Warn the user if its certain the matrix will not be invertible
  ## This does not guarantee that the matrix will be invertible
  ## NOTE : This function will process any input as it is assumed that
  ## an invertible matrix is being passed to it
  if(!(is.numeric(x) || is.complex(x) || is.logical(x))){
     warning("Matrix should be numeric or complex")
  }
  
  if(!(NROW(x) == NCOL(x))){
    warning("Matrix input should be a square matrix")
  }
  
  inv <- NULL #   Initiliase the inverse value to NULL
  
  # set() function
  #   - Update the stored matrix data
  #   - Clear the cached inverse matrix data 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get() function
  #   - Return the stored matrix data
  get <- function() x
  
  # setInverse() function
  # cache the data in argI as the inverse matrix data
  setInverse <- function(argI) inv <<- argI
  
  # getInverse() function
  # returns the cached inverse matrix data
  getInverse <- function() inv
  
  # return a list containing the function methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve solves the inverse matrix of a matrix stored in 'x'.
## 'x' is a cached matrix created with makeCacheMatrix.
## If the cached matrix already has cached data for the inverse matrix
## then cacheSolve returnes the cached data
## If there is no cached data available, then cacheSolve solves for
## inverse.

## The data returned is a matrix
## It is assumed that the matrix given x is always invertible
cacheSolve <- function(x, ...) {
  
  ## Retrieve the cached inverse data in 'x'
  inv <- x$getInverse()
  
  ## Return the cached inverse data in 'x' if it exists
  if(!is.null(inv)){
    message("Used cached value")
    return(inv)
  }
  
  ## If the inverse data does not exist then
  ## retrieve the matrix data in 'x'
  ## solve for the inverse of of that data
  mData <- x$get()
  inv <- solve(mData, ...)
  
  ## update the cached inverse data in 'x'
  x$setInverse(inv)
  
  ## Return the solved inverse of the matrix in 'x'
  return(inv)
}
