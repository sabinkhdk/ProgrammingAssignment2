## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # creates special "matrix" object that can cache its inverse matrix
  # initialize to store the inverse matrix
  xinv <- NULL
  # set(): to set matrix to object created by makeCacheMatrix()
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  # get() to return input matrix
  get <- function() x
  # setinv(): set inversed matrix in cache
  setinv <- function(inverse) xinv <<- inverse
  # getinv(): get inversed matrix from cache
  getinv <- function() xinv
  # return list with set, get, setinv, getinv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
  #Function to return matrix of x
  mat <- x$getinv()
  # checking if the matrix is inversed and already cached
  if(!is.null(mat)){
    message("getting cached data")
    return(mat)
  }
  # else calculate inverse
  rawmat <- x$get()
  mat <- solve(rawmat)
  x$setinv(mat)
  mat
}