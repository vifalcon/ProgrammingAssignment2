## These functions work together to access cached values and 
## to prevent unnecessary computation

## create a list to set value of matrix, get value of matrix, set
## value of inverse, get value of inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<-solve
  getinverse <- function() m
  list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)
}


## calculates inverse matrix, but first check if it has been 
## calculated already. If so, retrieves inverse from cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}


