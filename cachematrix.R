## Functions supporting keeping an inverse matrix function in cache memory.  

## Creates the matrix that supports cacheing.

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y){
    x <<-y
    invert<<- NULL
  }
  get<- function() x
  setInverse <- function(iData) invert <<- iData
  getInverse <- function() invert
  list(set= set, 
       get= get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## CacheSolve computes the inverse of the matrix returned by makeCacheMatrix.  
## If the inverse has been calculated then retrieves inverse from cache, otherwise
## computes the inverse.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  inverse<- x$getInverse()
  if (!is.null(inverse)){
    message("getting cached data")
    return (inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  return (inverse)


}
