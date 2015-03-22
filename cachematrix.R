## R script to create a matrix which can cache it's inverse.
## so there is no need to (costly) recalculate it as long as the matrix doesn't change

## makeCacheMatrix provides a list of 4 elements 
## containing functions to set and get the matrix (x)
## and to get and set the inverse of that matrix (im)

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function which looks for a cached inverse of the matrix and returns it if possible
## else it will calculate and return and store the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    im <- x$getinverse()
    if(!is.null(im)) {
      message("getting cached data")
      return(im)
    }
    data <- x$get()
    im <- solve(data)
    x$setinverse(im)
    return(im)
 
}

