## This file has two functions related to matrices inverse fetching and caching

## makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # Define the inverse matrix variable
  inverse <- NULL
  
  # Store the passed special matrix, and init the inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Return the stored special matrix
  get <- function() x
  
  # Cache the inverse of the special matrix
  setinverse <- function(i) inverse <<- i
  
  # Return the inverse of the special matrix. If not cached, it will return NULL, else the cached value
  getinverse <- function() inverse
  
  # Return the object with its lists of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
  

}

## cacheSolve - This function computes the inverse of the special "matrix" returned by 
#  makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # Get the inverse from the special matrix
  inverse <- x$getinverse()
  
  # If cached already, return it.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # Get the matrix from the object
  data <- x$get()
  
  # Inverse the matrix using solve
  inverse <- solve(data, ...)
  
  # Cache the result for later usages
  x$setinverse(inverse)
  
  # Return the result
  inverse  
}
