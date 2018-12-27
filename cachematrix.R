## These two functions make possible the calculation of the inverse of a matrix
## and the storage of the inverted matrix in cache so that it can be reused

## Creates a list of functions that make possible to set and get one matrix
## its inverted in caché

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function (y){
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list (setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse,
        getInverse=getInverse)
}


## Calculates the inverted of a matrix if it is not already in cache

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message ("getting cached data")
    return(inverse)
  }
  data <- x$getMatrix()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse    ## Return a matrix that is the inverse of 'x'
}
