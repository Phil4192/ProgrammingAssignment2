## this file contains two functions for programming assignment 2
## the "makeCacheMatrix" funtion creates a matrix-like object that can also cache its inverse
## the "cacheSolve" function takes this matrix-like object and checks if its inverse already exists
## if it already exits, the chached value is returned.  Otherwise, the inverse is computed, cached, and returned


## cacheSolve(x, ...) returns the inverse of x.  the cached value is used if available
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()  ##get and return inverse of x if already available
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }else{   ##otherwise compute inv and cache it
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
  }

}

## makeCacheMatrix() returns an objec that can hold a matrix and cached inverse of the matrix
##and also the methods to set/get the inverse and set/get the matrix
## use x$set() to set the matrix
## use x$get() to access the matrix
## use x$getinv() to access the inverse if it exists
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## init the inverse
  set <- function(y) {  ##set the value of matrix and init the inverse
    x <<- y
    m <<- NULL
  }
  get <- function() x  ## access the matrix
  setinv <- function(solve) m <<- solve   ## set the inverse of the matrix
  getinv <- function() m  ## access the matrix inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}