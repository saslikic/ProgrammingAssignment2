## makeCacheMatrix function creates a special matrix object, and then cacheSolve 
## function returns the inverse of the special matrix. 
## If the inverse of the matrix has already been calculated, cacheSolve will find it in
## the cache and return, and if it has not been calculated, it will calculate and 
## return the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  sas <- NULL
  set <- function(y) {
    x<<- y
    sas<<- NULL
  }
  get <- function() x
  setinverse <- function(solve) sas <<- solve
  getinverse <- function () sas
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x = matrix(), ...) {
  sas <- x$getinverse()
  if (!is.null(sas)) {
    message("getting cached inverse data")
    return(sas)
  }
  dat <- x$get()
  sas <- solve(dat, ...)
  x$setinverse(sas)
  sas
}
