## Here are some functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y){
    x <<- y
    xInv <<- NULL
  }
  get <- function (){
    x
  }
  setinverse <- function(inv){
    xInv <- inv
  }
  getinverse <- function(){
    xInv
  }
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  xInv <- x$getinverse()
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setinverse(xInv)
  xInv
}
