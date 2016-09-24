##Caching the inverse of Matrix:

## Matrix inversion is usually a costly computation  
## Caching the inverse of a matrix is better than computing it repeatedly

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed)
## the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
       inv <- x$getInverse()
  
  if(!is.null(inv))
        {
    message(" Getting cache data")
    return(inv)
  }
    mat <-  x$get()
  
  inv <- solve(mat,...)
  x$setInvers(inv)
  inv 
}
