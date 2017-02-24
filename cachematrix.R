## The function makeCacheMatrix creates a special matrix object. This 
## matrix object can store a matrix and caches the inverse of it.

## It has four functions 
## 1. get function to return the vector x
## 2. set fuction
## 3. setmean
## 4. getmean 

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


## cacheSolve Function computes the inverse of the special matrix that is 
## returned by makeCacheMatrix Function above. If the inverse has already been 
## calculated and the matrix has not been changed then the cacheSolve Function  
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m

        ## Returns a matrix that is the inverse of 'x'
}
