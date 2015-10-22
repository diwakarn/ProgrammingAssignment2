## This function caches the inverse of a matrix once it is computed 
## and can be reused. the inverse matrix is initially NULL and gets s
## set with the values after the inverse is calculatedby solve.
## This example shows lexical scoping 

## This function makes a matrix and a list of functions to get and set the 
## values of the inverse for future use 

makeCacheMatrix <- function(x = matrix()) {
  invx <- matrix()
  set <- function(y =matrix())
  { 
    x <<- y
    invx <- matrix()
  } 
  
  get <- function() x
  setinv <- function(solve) invx <<- solve 
  
  getinv <- function ()invx
  
  list(set=set,get=get,setinv=setinv,getinv=getinv)  
} 


## This function uses a matrix created by the makematrixcache function and
## provides teh inverse from the cache or after computing it and stores 
## it in the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv(); 
  
  if (!is.na(invx[1,1])) {
    message("getting cached data")
    return(invx)
  }
  mat <- x$get()
  invx <- solve(mat, ...)
  x$setinv (invx)
  invx 
}  

