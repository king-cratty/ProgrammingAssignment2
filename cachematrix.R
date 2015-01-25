## Two functions to cache a matrix and solve 
## m <- matrix(c(2,4), nrow=2, ncol=2)
## m[1,2] <- 4
## m[2,2] <- 2

## Create a special matrix that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special 'matrix'
## If the inverse of the matrix has already been computed
## then the matrix should be returned from the cache
## add to cache after computation

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if (!is.null(m)) {
    message('cached')
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}