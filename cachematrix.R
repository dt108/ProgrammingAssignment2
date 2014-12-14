# The most common technique to improve performace of a running program is to
# cache results of expensive operations. Expensive operations are defined as those that
# consume CPU cycles, memory and delay the processing.
# Matrix inversion is a costly computation depending on the size of the matrix.
# The following two functions help cache the matrix inverse so as to help improve 
# performance.

# makeCacheMatrix creates a list containing a function to
# 1. set value of the matrix
# 2. get value of the matrix
# 3. set inverse to be cached
# 4. get cached inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    #Initialize cached inverse to NULL
    cached_inverse <- NULL
    
    #set matrix whose inverse is to be cached
    set <- function(m) {
      x <<- m
      cached_inverse <<- NULL
    }
    
    #Return matrix
    get <- function() x
    
    #cache inverse of matrix
    setinverse <- function(inverse) cached_inverse <<- inverse
    
    #return cached inverse
    getinverse <- function() cached_inverse
    
    #lists function mappings
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)  
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve will retrieve the inverse from the cache and will not recompute the inverse.
# If inverse is not  cached, it computes the inverse, sets the value in the cache via
# setinverse function.
cacheSolve <- function(x, ...) {
  #Retrive cached inverse
  inv <- x$getinverse()
  
  #If cached inverse retrieved is not null return it
  if(!is.null(inv)) {
    message("returning cached data.")
    return(inv)
  } else { #inverse is not cached. Compute and cache it.
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    return(inv)
  }
}
