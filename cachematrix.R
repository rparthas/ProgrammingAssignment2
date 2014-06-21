##Creates a special matix which contains its corresponding inverse
##Creates the getter and setter methods for the acutal matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ##Setter and getter for actual matrix
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
  ##Setter and getter for inverse matrix
}

##Solves the inverse of matrix using solve function
##Before solving it checks if the input has inverse precomputed
##If it is it just returns the precomputed inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }  ##returns via cache
  data <- x$get()
  m <- solve(data)  ##computes the inverse
  x$setInverse(m) ##Sets the inverse back to the list
  m ##prints the result
}
