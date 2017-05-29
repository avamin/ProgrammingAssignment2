## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## goal of this assignment is to write a pair of functions that cache the inverse of a matrix.

## this assignment includes the following functions:
  
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## The first function, makeVector creates a list containing a function to

##  set the value of the matrix
##  get the value of the matrix
##  set the inverse of the matrix
##  get the inverse of the matrix

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


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

## test function
## x = rbind(c(1,2),c(3,5))
## call function to set matrix
## m <- makeCacheMatrix(x)
## check matrix value
##  m$get()
## this should return
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    5
## since its the first time, inverse of m is calculated and cached
##  cacheSolve(m)
## should return the inverse
##       [,1] [,2]
## [1,]   -5    2
## [2,]    3   -1
## get data from cache
## cacheSolve(m)
## should return this
## getting cached data
##      [,1] [,2]
## [1,]   -5    2
## [2,]    3   -1
