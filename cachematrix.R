## Put comments here that give an overall description of what your
## functions do

## This function is adjusted to matrix instead of vector and inverse is taken instead of mean

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

## This function is now adjusted to matrix instead of vector and solve instead of mean

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

# Test
a <- makeCacheMatrix(matrix(1:4,2,2))
a$get()
a$getInverse()
cacheSolve(a)
cacheSolve(a)
