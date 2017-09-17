## These functions will calculate the inverse of a matrix and
## save it to the cache so that when I calculate the the matrix inverse
## the saved value is brought back instead of repeating it.

## This function creates a special "matrix" object, which is really a list
## containing a function to
## 1. set the value of the matrix setmatrix
## 2. get the value of the matrix getmatrix
## 3. set the value of the inverse cacheinverse
## 4. get the value of the inverse getinverse

makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x and sub-function/method
  ## define the cache m 
  m <- NULL
  set <- function(y) {
    x <<- y ## assign the input y to the x in the parent environment
    m <<- NULL ## bring back m in the parent environment to NULL
}
  get <- function() x ## return the x
  setinverse <- function(inverse) m <<- inverse ## set the cache m equal to the inverse of the matrix x
  getinverse <- function() m ## return the inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the inverse of a special matrix made
## by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
