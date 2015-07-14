## The following functions shows the application lexical scoping in R
## The matrix inversion is performed using CacheSolve function and the inverse is stored in the cache for the first time
## Later when the same matrix inverse is called then the inverse is acquired from the cache

## makeCacheMatrix function creates a list of functions to calculate and store the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## This function calculates the inverse of a matrix, provided a list 'x'
## x is a list of functions containing the cached matrix inverse if the function was run once
## It gets the inverse from the cache if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
