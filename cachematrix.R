## These two functions allow a user to compute the inverse of
## a matrix and cache the result, so that if the result is
## needed later, it will not be re-computed, but simply retrieved
## from the cache.

## This function returns a list of functions that can store or
## retrieve the matrix whose inverse is to be computed. The functions 
## in the list can also compute and retreive said inverse. The
## matrix and its inverse are stored in a variable located inside
## the function's environment, so it cannot be affected by
## variable manipulations in the global environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes as its argument the list of functions returned
## in the previous function. Using those functions, it computes and
## retrieves a matrix's inverse. If the inverse
## has already been computed, this function retrieves that saved value.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()   ## Return a matrix that is the inverse of 'x'
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}