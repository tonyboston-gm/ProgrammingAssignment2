## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object, to cache its inverse
## makeCacheMatrix contains 4 functions: set, get, setmean, getmean.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

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



## This function returns the inverse of matrix. If the inverse has
## already been calculated, the function gets the inverse from the
## cache and skips the calculation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

## The function assumes that the matrix is always invertible.

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


## Example run:
## > x <- diag(5,3)
## > x
##      [,1] [,2] [,3]
## [1,]    5    0    0
## [2,]    0    5    0
## [3,]    0    0    5
## > m <- makeCacheMatrix(x)
## > m$get()
##      [,1] [,2] [,3]
## [1,]    5    0    0
## [2,]    0    5    0
## [3,]    0    0    5

## Get inverse matrix - No cache for first run
## > cacheSolve(m)
##      [,1] [,2] [,3]
## [1,]  0.2  0.0  0.0
## [2,]  0.0  0.2  0.0
## [3,]  0.0  0.0  0.2

## Get inverse matrix - Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data
##      [,1] [,2] [,3]
## [1,]  0.2  0.0  0.0
## [2,]  0.0  0.2  0.0
## [3,]  0.0  0.0  0.2
## >
## Calculate matrix product to get identity matrix
## > IdentityMatrix <- x %*% cacheSolve(m)
## getting cached data
## > IdentityMatrix
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1
## >

