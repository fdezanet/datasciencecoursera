## This script includes
## 1. the  function makeCacheMatrixc that creates a special "matrix" object that can cache its inverse
## 2. the function cachesolve that computes the inverse of the special "matrix" returned by makeCacheMatrix above
## 3. An example

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

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

## cacheSolve computes the inverse of the special "matrix" returned by function makeCacheMatrix 
## If the inverse has already been calculated, cachesolve retrieve the inverse from the cache
## The function assumes that the matrix supplied is always invertible.

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
  
}

## Example with a 2x2 matrix

a <- makeCacheMatrix(matrix(1:4,2)) 

b <- cachesolve(a)
b

b <- cachesolve(a)
b

