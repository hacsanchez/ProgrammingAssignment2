
### Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly (there are also alternatives to matrix inversion that 
## we will not discuss here). Your assignment is to write a pair of 
## functions that cache the inverse of a matrix.

## Write the following functions:
  
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix"  returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve 
## function in R. For example, if X is a square invertible matrix, 
## then solve(X) returns its inverse.

## For this assignment, assume that the matrix supplied is always 
## invertible.

## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { ## create the matrix "x" ##
  i <- NULL
  set <- function(s) {
    x <<- s
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the special "matrix"
## the special "matrix" which created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) { ## Return the inversed matrix of "t" ##
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i) 
  }
  data_mat <- x$get()
  i <- solve(data_mat, ...)
  x$setinverse(i)
  i
}
############################################################
## Sample run:
# > x = rbind(c(1,2,3), c(3,2,1), c(2,1,3))
# > mcm = makeCacheMatrix(x)
# > mcm$get()
##      [,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    3    2    1
##[3,]    2    1    3

## There are no values on cache in the first run
# > cacheSolve(mcm)
##             [,1]  [,2]       [,3]
## [1,] -0.41666667  0.25  0.3333333
## [2,]  0.58333333  0.25 -0.6666667
## [3,]  0.08333333 -0.25  0.3333333

## Retrieving from the cache in the second run
# > cacheSolve(mcm)
## getting cached data.
##             [,1]  [,2]       [,3]
## [1,] -0.41666667  0.25  0.3333333
## [2,]  0.58333333  0.25 -0.6666667
## [3,]  0.08333333 -0.25  0.3333333
############################################################




