##  Chaching the inverse of a matrix
##  The first function, "makeCacheMatrix" creates a special "matrix".
##  It contains a list of function to
## set() -set the value of the matrix
## get() -get the value of the matrix
## setinverse() -set the value of the inverse matrix 
## getinverse() -get the value of the inverse matrix

## Function to create special "matrix" object that chache its inverse
makeCacheMatrix <- function(x = matrix()) {
  mt <- NULL
  set <- function(y) {
    x <<- y
    mt <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mt <<- inverse
  getinverse <- function() mt
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Function to compute inverse of special matrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mt <- x$getinverse()
  if(!is.null(mt)) {
    message("getting cached data")
    return(mt)
  }
  data <- x$get()
  mt <- solve(data, ...)
  x$setinverse(mt)
  mt
}
