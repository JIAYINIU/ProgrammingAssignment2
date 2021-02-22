## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function.
## The function makeCacheMatrix create a special "matrix" object that 
## can cache the inverse. To be detailed, this function could
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## The function cacheSolve computes the inverse of the special "matrix"
## returned by the function makeCacheMatrix discussed above. If the inverse 
## has already been calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Return a function that is the inverse of matrix X
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
