## Functions that are used to create a special 'matrix' object  
## and cache's its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y){
    x <<- y
    Inverse <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) Inverse <<- solve 
  getsolve <- function() Inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## This function computes the inverse of a special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inverse <- x$getsolve()
  if(!is.null(Inverse)){
    message("getting cached data")
    return(Inverse)
  }
  data <- x$get()
  Inverse <- solve(data, ...)
  x$setsolve(Inverse)
  Inverse
}
