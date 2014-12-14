# The most common technique to improve performace of a ruuning program is to
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
    cached_inverse <- NULL
    set <- function(m) {
      x <<- m
      cached_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cached_inverse <<- inverse
    getinverse <- function() cached_inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)  
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve will retrieve the inverse from the cache and will not recompute the inverse.
# If inverse is not  cached, it computes the inverse, sets the value in the cache via
# setinverse function.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("returning cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
