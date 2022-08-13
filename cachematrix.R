## Cacheing the inverse of a matrix

## makeCacheMatrix() function creates an R object that stores the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = numeric()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve() function requires an argument that is returned by makeCacheMatrix() function to retrieve the inverse 
## from the cached value that is stored in the makeCacheMatrix() object's environment

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
