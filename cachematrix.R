## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# This function makes the special (caching) matrix.  It takes as an 
# input an ordinary matrix.  Although, it doesn't need a matrix 
# to initialize the object.  
#
# The function outputs four functions: set, get, setinverse, and getinverse
#
# The set function sets the matrix in the special matrix object, and 
# initializes the inverse matrix as Null.
#
# The get function gets the matrix (not the inverse one)
#
# The setinverse sets the inverse of the matrix, i.e. after it has 
# already been calculated (somewhere else, i.e. the cacheSolve function)
#
# The getinverse gets the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{

  inv <- NULL
  
  #-------------------------------------------------
  # Set function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #-------------------------------------------------
  # Get function: Get the matrix x that has been set
  get <- function() x
  
  #-------------------------------------------------
  # Set the inverse: 
  setinverse <- function(inverse) inv <<- inverse
  
  #-------------------------------------------------
  getinverse <- function() inv
  
  #-------------------------------------------------
  # Return the list of functions
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function:
#
# This function cache's the inverse of the special matrix x, which 
# is defined by the above function, makeCacheMatrix().  
#
# The function first checks if the inverse has already been calculated.  
# If it has, then the function gets it and returns it.  Otherwise, the 
# function gets the matrix, calculate the inverse, sets the inverse in the 
# special matrix object, and finally returns the inverse.  

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
