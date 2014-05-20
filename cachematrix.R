## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. Below are pair of functions that cache the inverse of a matrix

## function macheCaheMatrix creates a speacial cacheable matrix object. The assumption is a square matrix
## function cacheSolve creates the inverse of the matrix if not already done and caches the inverse for future use


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  xInverse <<- NULL
  
  ## gets the value for the matrix 
  get <- function() x
  
  ## sets the value for the matrix
  set <- function(y){
    x<<-y
    xInverse<<-NULL
  } 
  
  ## sets the inverse of the matrix
  setInverse <- function(inv) xInverse <<- inv
  
  ## gets the inverse o fhe matrix
  getInverse <- function() xInverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##get the inverse from cache
  xInverse <- x$getInverse()

  ##if cache is not empty, return cache
  if(!is.null(xInverse)) {
    message("getting cached data")
    return(xInverse)
  }
  
  ##cache is empty, get the original matrix and compute the inverse
  data <- x$get()
  xInverse <- solve(data)
  
  ##set the cache to the computed inverse
  x$setInverse(xInverse)
  
  ## return the inversed matrix
  xInverse
}
