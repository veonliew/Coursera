## This function creates a special "matrix" object that can cache its inverse.
## It takes a matrix as an input, set value of the matrix, get value of the 
## matrix, set inverse Matrix and get inverse Matrix.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix<-function(y){
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above, by using the solve function.  If the inverse has  
## already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)){
    message("getting cached invertible matrix")
    return(invMatrix)
  }
  matrixData <- x$getMatrix()
  invMatrix <- solve(matrixData, ...)
  x$setInverse(invMatrix)
  invMatrix
}
