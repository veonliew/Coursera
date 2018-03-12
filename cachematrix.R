
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
