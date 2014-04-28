## Programming Assignment 2
## 
## Two functions 
## makeCacheMatrix - creates a 'vector' holding a cache of
## a given square matrix and its inverse
## 
## cacheSolve - returns inverse if it has been calculated
## otherwise calculates it and returns the result.
##

makeCacheMatrix <- function(x = matrix()) {


  ## When created, set up NULL inverse
  inv <- NULL
  
  ## Functions:
  
  ## set a new value in the matrix
  ## updating source matrix, and resetting inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the matrix value
  get <- function() x
  
  ## set a new value for the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  ## get the inverse
  getinverse <- function() inv
  
  ## return the four functions in a list 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)


}


## 
##
## Function to obtain the inverse of matrix x
## 

cacheSolve <- function(x, ...) {

  ## retrieve the cached inverse
  ## if it's not null return its value
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## No cached version available
  ## Retrieve the matrix data and calculate the inverse
  ## then store it in the cache
  ## before returning its value
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


