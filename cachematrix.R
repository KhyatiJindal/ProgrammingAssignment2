makeCacheMatrix <- function(x = matrix()) {
  # this is an input fuction.
  inv <- NULL    #initialising the inverse as null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) 
    inv <<- inverse
  getinv <- function()  #function to get inverse
    inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) { #gets cache data
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv) #returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

# Calling the function and printing the result
m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
print(cacheSolve(m1))