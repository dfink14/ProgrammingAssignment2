## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #function to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #function to get the matrix 
  get <- function() x
  
  #function to set the inverse of matrix x using the solve function
  setinv <- function(solve) m <<- solve
  
  #function to get the value of matrix x
  getinv <- function() m
  
  #Create a list of all functions that can be used 
  #in the cacheSolve function
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  #Check if the inverse has been cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #If it has not been cached, find the inverse 
  #and return it. 
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


