## The following function creates a list containing a function that will set a matrix (that must be square), get the matrix, set it inverse and get it inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## The following function calculates the inverse of a (square) matrix given by the previous function. The calculation will be performed if the inverse of the matrix has not been calculated before. If the inverse has been calculated before, the function will obtain it from a cached variable.

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}

