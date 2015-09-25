## This fuction helps us in computing the inverse of the given matrix.
## If the matrix is quite huge then it can take time in computing the 
## inverse of the matrix, in this case,on the first call to the
## function the inverse is computed and when the subsequent call to 
## the function are made there after without any change to the matrix,
## the function returns the inverted matrix that was evaluated in the 
## first call to the function (i.e. the cached result)

## the function makeCacheMatrix() takes a square invertible matrix as
## input and return a list containing function that are used as the 
## input to cacheSolve() function, these functions are explained below
## the list contains the following functions :
##   1.setMat() to set the matrix
##   2.getMat() to get the matrix
##   3.setMatInv() to  set the inverse of the matrix
##   4.getMatInv() to retrieve the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  setMat <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  getMat <- function() x
  setMatInv <- function(inverse) matInv <<- inverse 
  getMatInv <- function() matInv
  list(setMat=setMat, getMat=getMat, setMatInv=setMatInv,
       getMatInv=getMatInv)
}


## This function cacheSolve() computes the inverse of the
## special "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix has not
## changed), then cacheSolve should retrieve the inverse from 
## the cache

cacheSolve <- function(x, ...) {
  matInv <- x$getMatInv()
  if (!is.null(matInv)){
    message("getting cached data for Matrix Inverse")
    return(matInv)
  }
  data <- x$getMat()
  matInv <- solve(data, ...)
  x$setMatInv(matInv)
  message("calculating inv now...")
  matInv
}
