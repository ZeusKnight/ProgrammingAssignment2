## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: his function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## Write a short comment describing this function
## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

#Experiment to try if it works
#square matrix
X <- matrix(rpois(25,3), nrow = 5)
cX <- makeCacheMatrix(X)
cX$get()
cacheSolve(cX)
cacheSolve(cX)
invX <- cacheSolve(cX)

#Experiment to try if it works
#rectangular matrix rows > cols
Y <- matrix(rpois(20,2), nrow = 4, ncol = 4)
cY <- makeCacheMatrix(Y)
cY$get()
cacheSolve(cY)
cacheSolve(cY)
invY <- cacheSolve(cY)

#Experiment to try if it works
#rectangular matrix rows < cols
Z <- matrix(rpois(20,1), nrow = 4, ncol = 4)
cZ <- makeCacheMatrix(Z)
cZ$get()
cacheSolve(cZ)
cacheSolve(cZ)
invZ <- cacheSolve(cZ)

#Experiment to try if it works
#multiplication must return identity or closer
invX %*% X 
X %*% invX
invY %*% Y 
Z %*% invZ 
