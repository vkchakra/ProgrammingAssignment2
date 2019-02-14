## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix as a parameter and returns another special matrix wich holds the inverse of the matrix in its cache

makeCacheMatrix <- function(x = matrix()) {
        
         inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function takes  the spceal matrix returned by makeCacheMatrix and computes its inverse. if already calculated, the inverse is returned from cache

cacheSolve <- function(x, ...) {
         i <- x$getinverse()
  if (!is.null(i)) {
          message("returning matrix from cache")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
