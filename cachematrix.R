## Create a special matrix object that stores its own
## inverse.

## This function creates, changes and stores the special matrix object.
## The function ASSUMES the matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function checks if the matrix object has a cached inverse
## if not compute the inverse and store it in the object.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  if (!(is.null(inv))) {
    
    print("Getting cached data.")
    return(inv)
  }
  
  mx <- x$get()
  inv <- solve(mx)
  x$setinv(inv)
  inv
        
}
