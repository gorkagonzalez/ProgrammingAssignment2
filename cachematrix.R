## In order to avoid calculating many times the inverse of a matrix which
## could be an expensive computation we use two functions, makeCacheMatrix so
## so as to be able to create and store the value of the inverse and another
## function cachesolve to compute the inverse or to reuse the cache inverse



## makeCacheMatrix creates a matrix object that can cache it's inverse.
## we do this by setting the value, getting the value, setting the value of the inverse
## and finally getting the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function () m 
  list (set =set, get = get, setinverse = setinverse, getinverse=getinverse)
  }

## This function calculates the inverse of the matrix returned by makeCacheMatrix.
## if the inverse has been already calculated, then
## we use the inverse from cache if not we calcualte the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)){
  message ("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  }
