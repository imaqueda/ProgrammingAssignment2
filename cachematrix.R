## The functions create a special "matrix" object that 
##cache the inverse and compute or retrieve the value 
##of the inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##Creates a list that contains a function to set and get
  ##the value of the matrix and the value of the inverse
  m <- NULL
  set <- function (y){
    x <<- y
    m <<- NULL
  }
  get <-function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}



cacheSolve <- function(x, ...) {
  ## Checks if the inverse has been calculated.
  ##Otherwise, calculates the inverse of the matrix
  ##and sets the value in the cache via setinv
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
