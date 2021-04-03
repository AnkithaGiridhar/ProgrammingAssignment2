## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function does the following:
#1. Set the value of the matrix.
#2. Get the value of the matrix.
#3. Set the value of the inverse of the matrix.
#4. Get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){return(x)}
  setinverse <- function(solve){ m <<- solve}
  getinverse <- function(){m}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function returns the inverse of the matrix from the makeCacheMatrix function.
# If the inverse has already been calculated, it does not calculate the inverse again. It gets the inverse of the cache and returns that after printing "getting cached inverse data".
# It then skips the rest of the computation.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}

