## We are creating a special matrix object that can store its inverse in cache.
## The object will be created by the makeCacheMatrix function.
## When calling the function chacheSolve, we'll return the cached inverse value
## if it has been cached previously or compute, cache and return it if not

## This functions creates the matrix-like object. It has four built-in functions:
## get: returns the original matrix object
## set: stores a new matrix object (and sets its cached inverse to null, 
## "resetint" it)
## setinverse: calculates the inverse of the original matrix object and stores 
## it in the cache (in the variable inv)
## getinverse: retrieves and returns the inverse of the matrix object stored in
## cache in the variable inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function() inv <<- solve(get())
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function tries to retrieve the cached inverse of a matrix-like object 
## created with the makeCacheMatrix function.
## If the inverse has already been computed and cached (so the object returned
## by the function getinverse() is not null), it returns this value
## If not (i.e., the value returned by getinverse() is null), it calculates and
## caches it by calling the function setinverse(), and returns its value

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  x$setinverse()
  x$getinverse()
}
