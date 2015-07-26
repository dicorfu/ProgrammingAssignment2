## write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

##  makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) invx <<-inverse
  getinverse <- function() invx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinverse()
  if (is.null(invx)) {
    ## the inverse matrix has not been computed, so compute it
    invx <- solve(x$get())
    x$setinverse(invx)
  } 
  ##return the inverse matrix
  return(invx)
  
}
