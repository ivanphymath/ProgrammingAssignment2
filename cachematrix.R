## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## In this function a set of function is created to be attached
## to a matrix

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  set_inv <- function(solve) inv <<- solve
  get_inv <- function() inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## Write a short comment describing this function
## In this function the inverse of a matrix is 'asked', if it
## is stored (!in.null would be true) it will return it,
# if it isnt stored yet, it will calculate it using the 
## function set_inv()

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- m$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_numeric <- m$get()
  inv <- solve(matrix_numeric, ...)
  m$set_inv(inv)
}
