## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  matInv <- x$getMatInv()
  if (!is.null(matInv)){
    message("getting cached data for Matrix Inverse")
    print(matInv)
  }
  data <- x$getMat()
  matInv <- solve(data, ...)
  x$setMatInv(matInv)
  print("calculating inv now...")
  print(matInv)
}
