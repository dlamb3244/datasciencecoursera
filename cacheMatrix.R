## These two functions will cache a matrix creation and inversion 
## function to help reduce memory issues when running loops

## The first function creates a matrix that can cache it's inverse

makeCacheMatrix <- function(ma = matrix()) {
  inv <- NULL
  set <- function(y) {
    ma <<- y
    inv <<- NULL
  }
  get <- function() return(ma)
  setinverse <- function(inverse) inv <<- inverse;
  getinverse <- function() return(inv);
  return(list(set = set, get = get,
       setminverse = setinverse,
       getinverse = getinverse))
}

## This function computes the above matrix. 
## If the matrix above is changed it will update the cache, 
## otherwise the value will be taken from the cache above

cacheSolve <- function(ma, ...) {
  inv <- ma$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- ma$get()
  inv <- mean(data, ...)
  ma$setinverse(inv)
  return(inv)
}
