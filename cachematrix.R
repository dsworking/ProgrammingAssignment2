## Pair of functions that enable computing the inverse of a matrix, using
## caching to prevent repetition of the costly inverse computation


## makeCacheMatrix: Create a special matrix object that can cache its
## inverse, including functions to set and get the object, as well as set
## and get the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(solve) i <<- solve
  getInv <- function() i
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve: Compute the inverse of the special matrix object created by
## makeCacheMatrix, returning the cached inverse if it has already been
## computed
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
