## This file contains two functions to compute and cache
## the inverse of a matrix

## The first function makes a matrix object which has the options:
## get/set matrix (to save and get the matrix)
## get/set inverse (to save and get the inverse)

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invM <<- inverse
  getinverse <- function() invM
  ## here we ensure that the subfunctions can be called.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function returns the inverse of x
## first check whether it already exists, if so return
## otherwise compute and return

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    invM <- x$getinverse()
    if(!is.null(invM)) {
      message("getting cached data")
      return(invM)
    }
    data <- x$get()
    invM <- solve(data)
    x$setinverse(invM)
    return(invM)
}
