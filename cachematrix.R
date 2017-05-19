## This function creates a special "matrix" object that can cache its inverse

##  set, get, getinv, and setinv are the methods created for this matrix
##  set - set the matrix value and also set the cached inverse value to NULL
##  get - return the matrix
##  setinv - save the matrix inverse
##  getinv - return the matrix inverse

##  obs:  makeCache return this methods as a list

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix.
## If the inverse is already cached, return this value
## If isn't cached, do the calculation, cache and return this calculated value

cacheSolve <- function(x, ...) {
## Get the value in cache
    m <- x$getinv()

## If it has cached value it returns this value
    if(!is.null(m)) {
      message("getting cached inverse")
      return(m)
    }

## If nothing is cached, retrieve the matrix and calculate the inverse
    data <- x$get()
    message("calculating inverse")
    m <- solve(data, ...)

##Before returning the value saved in the cache
    x$setinv(m)
    
    m
}
