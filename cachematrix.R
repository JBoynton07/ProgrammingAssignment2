## When a matrix is input into makeCacheMatrix, the function will clear its cache of any previous values as a base and in set. It will then assign the matrix to x and record it in get. Then it will calculate the inverse of x and assign it to m in setinverse. it is stored in geninverse. Then when cacheSolve is run, it will first check for if there is a cached value in getinverse from makeCacheMatrix and store it in m.local. If there is, it will declare its message and return the value and end the function. If there is not, it will: retrieve the value of the matrix set in get; calculate and store the inverse function in m.local.inverse; calls from setinverse so that the solve function will be stored in m for future use; then print the Inverse Matrix. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL					
   set <- function(y) {			
      x <<- y					
      m <<- NULL				 
   }               				     
   get <- function() x       			
   setinverse <- function(solve) m <<- solve	
   getinverse <- function() m			
   list(set = set, get = get,			
        setinverse = setinverse,
        getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(mcm, ...) {		
   m.local <- mcm$getinverse()				 
   if(!is.null(m.local)) {			
      message("getting cached data")
      return(m.local)				
   }						
   data <- mcm$get()				
   m.local.inverse <- solve(data, ...) 	
   mcm$setinverse(m.local.inverse)		
   m.local.inverse				
}