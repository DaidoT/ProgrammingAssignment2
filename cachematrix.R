## Put comments here that give an overall description of what your
## functions do

## Function 'makeCacheMatrix' - This function creates a special matrix
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(solve)inv <<- solve
  getInverse <- function()inv
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}


## Function 'cacheSolve' - This function computes the inverse of the
## matrix return by 'makeCacheMatrix'. If the inverse has already
## been calculated, and not changed, then it retrieves the inverse
## from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getInverse()
    if(!is.null(s)) {
      message("Getting cached data")
      return(inv)
    }
    data <- x$get()
    s <- solve(data,...)
    x$setInverse(s)
    s
}
