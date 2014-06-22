## MakeCacheMatrix defines getters and setters for accessing 
## matrix and it's inverse

## CacheSolve checks if inverse is already in cache. 
## Adds value to cache if it wasn't there


## Function defines four functions for getting and setting matrix x and 
## it's inverse. Returns a list containing these functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## Function cacheSolve checks if x has inverse value in cache and returns it
## If value not in cache, solves the inverse value, sets it to cache and returns it

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    ## Value found from cache
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  ## Set value to cache and return it
  x$setinverse(m)
  m
}
