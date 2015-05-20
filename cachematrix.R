## Put comments here that give an overall description of what your
## functions do

##set the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
##get the value of the matrix
  get <- function() x
  
##set the value of the inverse of matrix
  setinverse <- function(inverse) inv <<- inverse
  
##get the value of the inverse of matrix
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ## If the inverse has already been calculated (and the matrix has not changed), 
  ## then the cachesolve should retrieve the inverse from the cache.
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setinverse(inv)
  inverse
  
  
}
