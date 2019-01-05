##  Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation
## so it better caching the inverse of a matrix rather than compute it repeatedly


## makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  InvM <- matrix()
  set <- function(y) {
    x <<- y
    InvM <<- matrix()
  }
  get <- function() x
  setInv <- function(Inv) InvM <<- Inv
  getInv <- function() InvM
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}



## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InvM <- x$getInv()
  if(!all(is.na(InvM))) {
    message("getting cached data")
    return(InvM)
  }
  data <- x$get()
  InvM <- solve(data, ...)
  x$setInv(InvM)
  InvM
}
