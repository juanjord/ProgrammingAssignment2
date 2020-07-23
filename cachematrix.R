
## The first function creates an object that stores a matrix and the inverse of this matrix.
## While the second function uses an argument to retrieve the inverse of the matrix 
## using the stored value in the first object.

## This function sets and gets the values of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # An object for the inverse is set null to use it later
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) # A list to call the arguments separately
}

## This function retrieves the inverse matrix. If the matrix hasn't changed, then it would
## retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("get cached data") # Get the cached data in case the object "inv" is not null
    return(inv)
  }
  # Getting the inverse in case it is null
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}


