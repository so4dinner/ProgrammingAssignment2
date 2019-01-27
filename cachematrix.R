## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a list containing a function to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of inverse of the matrix
# 4. Get the value of inverse of the matrix

## Assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv =getinv)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of the matrix by
# Checking whether the inverse has been computed already.
# If so, it gets the result.
# If not, it computes the inverse and sets the value in the cache
# with the setinverse function

## Assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)  ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
