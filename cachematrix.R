## Put comments here that give an overall description of what your
## functions do

## makeCachematrix stores a matrix in 'x' and its inverse in 'i'.
## It also provides four accessor functions to get and set the matrix
## and to get and set the inverse. This is, in essences, the same as
## the example function (makeVector) from the assignment description.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(inverse) {
    i <<- inverse
  }
  getinverse <- function() {
    i
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of the given matrix.
## The function first checks if the matrix already has a value for the
## inverse, if it does, this is returned (saving computing power and time),
## if not, it calculates the inverse and stores this for future use. This is,
## in essence, the same as the example function (cachemean) from the 
## assignment description.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  message("calculating inverse")
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
