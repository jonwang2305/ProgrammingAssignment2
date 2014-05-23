## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix creates a list defining the set, get functions for a matrix and its inverse
#cacheSolve uses the special matrix set by makeCacheMatrix as its argument and either finds the cached inverse and returns it, or calculates the inverse itself, caches it, and returns it


## Write a short comment describing this function
#stores a special matrix containing a list to
#1.set value of matrix 2.get value of matrix 3.set value of inverse 4.get value of inverse
#and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  #will store cached inverse matrix
  inv <- NULL
  
  #set matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  #get matrix
  get <- function() x
  
  #set and get inverse matrixes
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  #return matrix
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
  
  
## Write a short comment describing this function
#first checks for cached inverse
# -if available, it is returned
# -if not available, it computes the inverse, caches it, and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  
  #if the inverse is cached, return it
  if (!is.null(inv)){
    message ("getting cached data")
    return(inv)
  }
  
  #if the inverse is not cached, compute, cache, and return it
  data <- x$get()
  #with only one argument, the 'solve' function calculates the inverse of its argument
  inv <- solve(data, ...) 
  x$setinverse(inv)
  return(inv)
}
