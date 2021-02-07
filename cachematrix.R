## Combined together, the following two functions allow us to avoid the time-
## consuming operation of repeatedly calculating the inverse of matrices. The
## first function allows the user to define the matrix (or matrices) the inverse
## of which will be calculated repeatedly. The second function either computes
## or retrieves (from the cache) the inverse of the matrix (matrices) defined 
## through the first function.


## This first function, makeCacheMatrix, creates a list containing a function
## to:
##        1. set the value of the matrix the inverse of which we want to cache
##        2. get the value of the matrix the inverse of which we want to cache
##        3. set the value of the inverse matrix we want to cache
##        4. get the value of the inverse matrix we want to cache

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The following function calculates the inverse matrix which we wanted to cache
## in the function above. The function first checks whether the inverse of the
## matrix has already been calculated. If so, it gets the inverse from the
## cache and skips computations. Otherwise, it calculates the inverse of the 
## specified matrix and sets the value of the inverse in the cache via the 
## setmean function.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse ## Return a matrix that is the inverse of 'x'
        
}
