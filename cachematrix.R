## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly . This code writes a pair of functions that caches the 
## inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  
  ## Sets the original matrix value
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  
  ## Sets the original matrix value
  get <- function() x
  
  ## Sets the cached inverse value
  setinvmat <- function(invmat) invm <<- invmat
  
  ## Gets the cached inverse value
  getinvmat <- function() invm
  
  ## Returns the special vector
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Try and retreive cached matrix
  invm <- x$getinvmat() 
  
  ## Return cached matrix if cached value exists
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  
  ## If cached value not found, calculate it, and store it in cache
  data <- x$get()
  invm <- solve(data, ...) ## Compute inverse matrix
  x$setinvmat(invm) ## Set value in cache
  invm
}
