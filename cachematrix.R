## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly
## Below are two functions that are used to create a special object that
## stores a matrix and could cache its inverse.

## This function creates a special "matrix" object that can cache its inverse
## Creation is only successful when the entity passed in is a squared matrix
## It is really a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse cache
## 4. get the cached inverse
makeCacheMatrix <- function(x = matrix()) {
  checkArgument(x)
  inv <- NULL
  set <- function(y) {
    checkArgument(y)
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## make sure the argument is a squared matrix
checkArgument <- function(x) {
  if (!is.matrix(x))
    stop("Argument has to be a matrix")
  if (nrow(x) != ncol(x))
    stop("Cannot create for non squared matrix")  
}
