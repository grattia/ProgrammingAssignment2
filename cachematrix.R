## This script contains two functions: makeCacheMatrix and cacheSolve to avoid recalculation of time
## consuming tasks. This function take advantage of Lexical scoping in R.
##
## this function is based on makeVector and cachemean original functions.

## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing functions listed below:
## set: set the value of the matrix
## get: get the value of the matrix
## setsolve: set the value of the inverse matrix
## getsolve: get the value of the inverse matrix
##
## also save the matrix and inverse matrix data in memory.
##
## how to test it:
## consider a matrix 2x2 
##      [,1] [,2]
## [1,]    4    3
## [2,]    3    2
##
## myMatrix <- makeCacheMatrix(matrix(c(4, 3, 3, 2),2,2))
## cacheSolve(myMatrix)
## validation: the folloging operarion should return the identity matrix
## > myMatrix$get() %*% myMatrix$getsolve()
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##  execute cacheSolve again, to see the message "getting cached data"
## cacheSolve(myMatrix)


makeCacheMatrix <- function(x = matrix()) {
   inverseMatrix <- NULL
   set <- function(y) {
      x <<- y
      inverseMatrix <<- NULL
   }
   get <- function() x
   setsolve <- function(solve) inverseMatrix <<- solve
   getsolve <- function() inverseMatrix
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}

## cachesolve calculate the inverse matrix of 'x'. it validate whether the inverse was calculated before
## to avoid recalculation.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'

   inverseMatrix <- x$getsolve()
   if(!is.null(inverseMatrix)) {
      message("getting cached data")
      return(inverseMatrix)
   }
   data <- x$get()
   inverseMatrix <- solve(data, ...)
   x$setsolve(inverseMatrix)
   inverseMatrix
}