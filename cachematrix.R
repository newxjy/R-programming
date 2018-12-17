## This function creates a special "matrix" object that can cache its inverse.
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.
## The following is a pair of functions that cache the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
                  x <<- y
                  inv <<- NULL
          }
          get <- function() x
          setInverse <- function(inverse) inv <<- inverse
          getInverse <- function() inv
          list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)
}


## cacheSolve is a computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          inv <- x$getInverse()
          if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
          }
          data <- x$get()
          inv <- solve(data,...)
          x$setInverse(inv)
          inv
}
