## Put comments here that give an overall description of what your
## functions do
## The two functions calculate the inverse of a matrix or get it from the cache, for a given matrix

## Write a short comment describing this function
## This function contains four functions that create a matrix object that can cache its inverse
## set changes the x stored in the main function, get returns the x vector, getinverse and setinverse get and store the value of the input in m.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
## This function computes the inverse of the matrix returned by the previous function.
## If the inverse has already been calculated, it is retrieved from the cache.
## If it has not been calculated data retrieves the matrix, then m finds the inverse, then it is stored in m in the previous function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
