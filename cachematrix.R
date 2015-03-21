## Put comments here that give an overall description of what your
## functions do
# This function make a list with four functions for store the matrix and its inversed matrix
# set: set a vector for store matrix
# get: get the matrix
# setmatrix: set the vector for store inverse matrix
# getmatrix: store the inverse matrix

 makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
		 # set a vector for store matrix
         set <- function(y) {
                 x <<- y
                 m <<- NULL
         }
		 # return the matrix
         get <- function() x
		 # set the variable for store the value
         setmatrix <- function(initial) m <<- initial
		 # get the solved matrix
         getmatrix <- function() m
         list(set = set, 
              get = get,
              setmatrix = setmatrix,
              getmatrix = getmatrix)
 }

## Write a short comment describing this function
# this function is making the inverse matrix of the matrix stored in makeCacheMatrix 
 cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
         # get the solved matrix vector
		 m <- x$getmatrix()
		 # check whether the inverse matrix is existed or not
         if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
         }
		 # get the matrix, make it inverse
         inverse <- x$get()
         m <- solve(inverse, ...)
		 # stored in solved matrix vector as the final result
         x$setmatrix(m)
		 # return the solved matrix
         return(m)
 }
