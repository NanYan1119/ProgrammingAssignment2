## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function creates a special matrix,
## which is a list containing a function to
## 1. set the value of the square invertible matrix
## 2. get the value of the square invertible matrix
## 3. set the inverse of the square invertible matrix
## 4. get the inverse of the square invertible matrix
## The input of the makeCacheMatrix function is a square invertible matrix

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


## Write a short comment describing this function

## The cacheSolve function calculates the inverse of the special "matrix" created with the makeCacheMatrix function.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data using the solve function in R
## and sets the inverse in the cache via the setinverse function.
## The function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached inverse of matrix")
    return(i)
  }
  
  mat <- x$get()
  
  i <- solve(mat, ...)
  
  x$setinverse(i)
  
  ## i is the inverse of x
  i
}
