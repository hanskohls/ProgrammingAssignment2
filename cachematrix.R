## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(set_inv) inv <<- set_inv
  getinv <- function() inv
  
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)){
    message('Returning inverse from cache.')
    return(inv)
  }
  message('Calculating and setting inverse of matrix')
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinv(inv)
  inv
}
