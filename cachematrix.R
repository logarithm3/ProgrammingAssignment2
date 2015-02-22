#     makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## caching the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      #2.  get the value of the matrix
      get <- function() x
      #3.  set the value of the inverse
      setInverse <- function(inverse) m <<-inverse
      #4.  get the value of the inverse
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
      
}

#     cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
      m <- x$getInverse()
      
      #If the inverse has already been calculated (and the matrix has not changed),
      #then the cachesolve should retrieve the inverse from the cache.
      if ( ! is.null(m)) {
            print("getting cached data")
            return(m)
      }
      #Computing the inverse of a square matrix can be done with the solve function in R.
      m <- solve(x$get())
      x$setInverse(m)
      m
}



