## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

## The two functions below are used to create a special object that stores a
# matrix and caches its inverse. 

## The function "makeCacheMatrix" creates list of functions that pertain to
## a matrix. The list contains functions to 1) set matrix value 2) get matrix
## value 3) set matrix inverse 4) get matrix inverse. 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The function "cacheSolve" calculates the inverse of the special matrix made
## by "makeCacheMatrix". It uses the solve() function to calculate inverse.
## It first checks whether a given matrix already has its inverse computed, in
## which case it returns the previously computed inverse and does not continue. 
## If the inverse has not been computed, it does so and then sets the inverse
## for that matrix via "setinverse".

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

