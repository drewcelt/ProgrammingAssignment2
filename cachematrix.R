## These functions are able to cache the inverse of a matrix after
## calculating it or return the inverse if it has already been calculated.

## This function contains other functions that allow the user to cache the 
## inverse of a given matrix once it is calculated by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      } 
      get <- function() x
      setinv <- function(inv) i <<- inv
      getinv <- function() i
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## This function returns the inverse of the given matrix if it has already
## been calcultated, otherwise it calculates its inverse and stores it. 

cacheSolve <- function(x, ...) {
      i<-x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      } else {
      mat <- x$get()
      i <- solve(mat, ...)
      x$setinv(i)
      i
      }
}
