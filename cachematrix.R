## Saves matrix in cache
makeCacheMatrix <- function(x = matrix()) {
  ## inverse is reset
  m <- NULL
  ## Used to change matrix to a new one
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ## retrieves original matrix
  get <- function() x
  ## sets inverse to m
  setinverse <- function(solve) m <<- solve
  ## Returns inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Returns and calculates the inverse(solve) of the matrix from makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## Gets the inverse from makeCacheMatrix
  m <- x$getinverse()
  ## If the inverse is already generated it will print the message and return the inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Gets the matrix from makeCacheMatrix
  data <- x$get()
  ## Uses solve function to create the inverse
  m <- solve(data, ...)
  ## sets the m/inverse in the makeCacheMatrix to the inverse matrix generated 
  x$setinverse(m)
  #prints the inverse
  m
}
