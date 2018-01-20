## Put comments here that give an overall description of what your
## functions do

## The overall objective is to compute the inverse of a matrix and
## store ot locally to save computation time if it needs to be accessed repeatedly 
##Assumption in this case is the matrix is always invertible
## Concept is there are 2 divisions existing, get and set, using get we would be able to 
## retrieve the stored value and set would be used to set the inverse value to the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set,get = get,setinv = setinv,getinv = getinv)
}


## Write a short comment describing this function

## Function to check if the inverse has already been computed, if yes, then it directly can return the value, 
## else it calls from set. Here the function solve is used to get the inverse of the matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("get cached data")
    return(inv)
  }
  val <- x$get()
  inv <- solve(val, ...)
  x$setinv(inv)
  inv
}
