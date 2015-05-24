## The following two functions implement a simple mechanism to cache the inverse of a matrix

## makeCacheMatrix create a special object (matrix) that is able to cache its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # the i variable will contain the cached inverse matrix
  i <- NULL
  
  # setter function
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # getter function
  get <- function() x
  
  # function to set the inverse of x
  setinverse <- function(inverse) i <<- inverse
  
  # function to get the inverse of x
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## this function returns the inverse of a matrix x
## It first checks if the inverse matrix is already cached. 
## If not, the function will cache the inverse of x for subsequent uses

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  # get the inverse from x
  i <- x$getinverse()
  
  # is it cached? Yes, return the inverse of x and exit the function
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # is it cached? No. 
  
  # get the value of x
  data <- x$get()
  
  # get the inverse of x using the "solve" function
  i <- solve(data, ...)
  
  # cache the inverse in x
  x$setinverse(i)
  
  # return the inverse of x
  i
}
