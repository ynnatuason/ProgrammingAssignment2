## These functions serve as compliance to the Data Science: R Programming
## Week 3 Programming Assignment

## This function makes a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  a  <- NULL                                    ## starts a as NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) a <<- inverse ## assignment of value of inv
  getInverse <- function() a                    ## gets the value of inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function is returned by makeCacheMatrix which is written above and it
## solves the inverse of the special "matrix". 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  a <- x$getinverse()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  matrix <- x$get()
  a <- solve(matrix, ...)
  x$setinverse(a)
  a
}
