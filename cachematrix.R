## Author: Steven Cheng
## Date: Feb-21-2016
## Description: Caching the Inverse of a Matrix
## in order to cache potentially time-consuming computations
## while inversing a Matrix

## Variables x and m
## x for the matrix to be processed
## m for the result of function after processing
makeCacheMatrix <- function(x = matrix()) {
  
  ## Definition inside the function    
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Get what to process
  get <- function() x
  
  ## Set the result to setinverse
  setinverse <- function(inverse) m <<- inverse
  
  ## Return the result
  getinverse <- function() m
  
  ## makeCacheMatrix returns a list
  ## elements of the list has been defined above
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## input of cacheSolve must be a makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    ## check if Cache is empty and return value
    message("getting cached data")
    return(m)
  }
  
  ## Cache empty, so transform the matrix
  data <- x$get()
  
  ## t() function
  m <- t(data)
  
  ##use setinverse
  x$setinverse(m)
  
  ## return the result
  m
}

## input example as below
MatrixA <- matrix(1:6,nrow=2,ncol=3)
cachedMatrixA = makeCacheMatrix(MatrixA)
cacheSolve(cachedMatrixA)
cacheSolve(cachedMatrixA)
