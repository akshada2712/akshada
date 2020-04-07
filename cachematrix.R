## Caching the inverse of matrix

##This function creates a matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function () i
  list(set =set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  
}


## This function computes inverse of special matrix returned by makeCacheMatrix


cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){
    message("GETTING CACHED DATA")
    return (i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setInverse(i)
  i
}

