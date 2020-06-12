## Put comments here that give an overall description of what your
## functions do

## returns a list containing functions to set and get the matrix, set and get the inverse
## this function is used as an input to the second main function below.

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) a <<- inverse
  getinv <- function() a
  list(setmat = setmat,
       getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## function below returns the inverse of matrix that we input in makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  a <- x$getinv()
  #if inverse is already calculated, it gets it from the cache and skips computation
  if (!is.null(a)) {
    message("Getting Cached Data!!")
    return(a)
  }
  data <- x$getmat()
  i <- solve(data, ...)
  x$setinv(a)
  i
}
