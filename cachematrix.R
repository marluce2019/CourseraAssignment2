## Two functions used to compute the inverse of a square matrix
#Create  "matrix" object and get inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  matM <- NULL
  set <- function(y) {
    x <<- y
    matM <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matM <<- inverse
  getinverse <- function() matM
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  matI <- x$getinverse()
  if (!is.null(matI)) {
    message("getting cached data")
    return(matI)
  }
  data <- x$get()
  matI <- solve(data, ...)
  x$setinverse(matI)
  matI
}
