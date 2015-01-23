## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # to initialize the variable
  set <- function(y) { # sets initial values to inverse variable in the funciton env
    inv <<- NULL # initialize the inv variable in new environment
    x <<- y # store matrix x in new environment
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse # when called, sets inverse matrix into inv variable in the env
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # recall getinverse funciton within x, set it to variable inv
  if(!is.null(inv)) { # if inv variable contains value, return 'cached' copy
    message("getting cached data")
    return(inv) 
  }
  matrix <- x$get()
  inv <- solve(matrix) # solve for inverse matrix
  x$setinverse(inv) # recall setinverse function to store in the env.
  return(inv)
}

