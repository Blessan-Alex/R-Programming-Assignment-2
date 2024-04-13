## These functions provide a mechanism to efficiently compute and retrieve the inverse of a matrix, avoiding redundant calculations.

## This function creates a special matrix object and stores it in a cache. It allows users to create a matrix and store it for later use. If no matrix is provided as an argument, it defaults to an empty matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize a list to store the matrix and its inverse
  cache <- NULL
  
  ## Setter function to set the matrix
  set <- function(y) {
    x <<- y
    cache$inverse <<- NULL  # Clear the cached inverse when the matrix is updated
  }
  
  ## Getter function to retrieve the matrix
  get <- function() {
    x
  }
  
  ## Setter function to set the inverse of the matrix
  setinverse <- function(inverse) {
    cache$inverse <<- inverse
  }
  
  ## Getter function to retrieve the inverse of the matrix
  getinverse <- function() {
    cache$inverse
  }
  
  ## Return a list containing the getter and setter functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function retrieves the inverse of a matrix from cache if available; otherwise, it calculates the inverse and caches it for future use. It provides a mechanism to efficiently compute and retrieve the inverse of a matrix, avoiding redundant calculations.

cacheSolve <- function(x, ...) {
  ## Retrieve the cached inverse if available
  inverse <- x$getinverse()
  
  ## If the cached inverse exists, return it
  if (!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  
  ## If the cached inverse doesn't exist, calculate the inverse
  mat <- x$get()
  inverse <- solve(mat, ...)
  
  ## Cache the calculated inverse for future use
  x$setinverse(inverse)
  
  ## Return the calculated inverse
  inverse
}
