## the below function creates a special matrix object that can cache its inverse
## below function returns a list containing functions to set and get the matrix, set and get the inverse
## this function is used as an input to the second main function below.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL ##intialises the inverse property
  ##setting the matrix 
  setmatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ##method to get the matrix 
  getmatrix <- function() x ##returns matrix 
  ##method to set the inverse of the matrix 
  setinverse <- function(inverse) inverse <<- solvematrix
  ##method to get the inverse of the matrix 
  getinverse <- function() inverse
  ##return a list of the methods
  list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function below returns the inverse of special matrix that we input in makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  ##if inverse is already calculated, the function retrieves the inverse from the cache and skips computation
  if (!is.null(inverse)) {
    message("Getting Cached Data!!")
    return(inverse)
  }
  data <- x$getmatrix() ##gets matrix from our object
  inverse <- solve(data, ...) ##calculates inverse using matrix multiplication 
  x$setinverse(inverse) ##set the inverse to the object
  inverse ##returns the matrix 
}
