## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix geterates a list with four functions:
##    set - stores a matrix
##          invalidates a cached inverse matrix
##    get - returns the matrix
##    setinv - stores a matrix as inverse of the matrix
##    getinv - returns the inverse of the matrix
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


## cacheSolve takes a list of functions as generated in makeCacheMatrix
## when called it will return the inverse of the matrix stored in x$get()
## if the inverse has not been calculated it will call solve
## otherwise it will return the cached value.
cacheSolve <- function(x, ...) {
  
  ## Get the value stored in 'inv'
  inv <- x$getinv()
  
  ## check if the cached value is not null and return it.
  if (!is.null(inv)){
    message(' ... Returning inverse from cache.')
    return(inv)
  }
  
  ## if the stored inverse is null, calculate it. 
  message(' ... Calculating and setting inverse of matrix')
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinv(inv)
  inv
}

## the sample timer will generate a random n by n matrix
## it's a sample how the cachesolve function can be called. 
sampleTimer <- function(n=1000){
  message('Generating Matrix ...')
  m <- replicate(n,rnorm(n))
  message('Creating Cached Matrix')
  m_cache <- makeCacheMatrix(m)
  message('Solving the matrix twice ...')
  m_inv = cacheSolve(m_cache)
  m_inv = cacheSolve(m_cache)
}
