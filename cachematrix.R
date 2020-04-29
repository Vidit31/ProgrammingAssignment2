## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##here, the function makeCacheMatrix creates a list of function that can set the matrix,
##get the matrix, set the inverse for later call and get the inverse of the ,atrix by calling 
##getinverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## here, x is the special list formed by function above for caching the inverse of the matrix
##here the function cacheSolve checks whether the inverse has already been calculated or calculated,
##If calculated, it retrieves its inverse from cache and dont compute it again, otherwise it computes
## the inverse and then set it in the list for future caching.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setinverse(m)
  m
## Return a matrix that is the inverse of 'x'
}
