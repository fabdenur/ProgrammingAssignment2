## the first function gets and sets matrix to be inverted
## the second functions checks whether the matrix has already been inverted and stored; if yes,it retrieves the inverted matrix
## otherwise it calculates the inverted matrix

## stores matrix to be inverted via a list of functions to get and set the inputed matriz

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinvmat <- function(invmat) m <<- invmat
      getinvmat <- function() m
      list(set = set, get = get,
           setinvmat = setinvmat,
           getinvmat = getinvmat)
}


## checks if inverted matrix has already been stored; if yes, retrieves stored matrix; otherwise calculates inverted matrix

cacheSolve <- function(x, ...) {
      m <- x$getinvmat()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinvmat(m)
      m
}
