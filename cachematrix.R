## Write a short comment describing this function
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly.
## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
          }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the inverse of the 
## special "matrix" created with the above function.
## However, it first checks to see if the inverse has 
## already been calculated.  If so, it gets the mean from 
## the cache and skips the computation.  Otherwise,  it
## calculates the mean of the data and sets the value of 
## the mean in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)) {
        message("getting cached data")
        return(m)
    
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  m
}
