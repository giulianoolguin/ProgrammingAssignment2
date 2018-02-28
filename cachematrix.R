
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) { ##redefines matrix "x" and overwrites the "temp" value in the parent environment
          x <<- y
          i <<- NULL
  }

  get <- function() x

  setinverse <- function(inverse) i <<- inverse ## setinverse() defines a new value to 'i'
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("Cached Data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)  ## cacheSolve() calculates the inverse of the  matrix 
  x$setinverse(i)
  i
}
