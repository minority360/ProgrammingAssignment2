## These functions are used to cache the inverse of a matrix


## this function creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set is a function that changes the vector stored in the main function
  set <- function(ym)
  {
        cm  <<- ym    ## substitutes the matrix cm with ym (the input) in the main function
        inv <<- NULL     ## restores to null the value of the inv
  }
  
  ## get is a function that returns the matrix cm stored in the main function.
  ## doesn't require any input
  get <- function()
        cm
    
  ## getInverse and setInverse are functions similar to get and set
  ## they simply store the value of the input in a variable inv into the 
  ## main function makeCacheMatrix (setInverse) and return it (getInverse)
  ## <<- operator is used to assign a value to an object in an envmt that is
  ## different from the current envmt
  setInverse <- function(solve)
        inv <<- solve
  
  getInverse <- function()
        inv
  
  ##  to store 4 function in the function makeCacheMatrix
  list( set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## This function computes the inverse of the special 'matrix' returned 
## by makeCacheMatrix function. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## verify the value of inv, stored previously with getInverse
  inv <- cm$getInverse()
  
  ## if exists in memory, returns a message and value inv but not necessary
  if(!is.null(inv)){  
    message("getting cached data")
    return(inv)
  }
    
  ## data gets the matrix stored with makeCacheMatrix
  data <- cm$get()
  
  ## inv calculates the inverse of the matrix
  inv <- solve(data, ...)
  
  ## cm$setInverse(inv) stores inv in the object generated assigned with makeCacheMatrix
  cm$setInverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
        
}
