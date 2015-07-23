## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix".
#This function does the following.
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

#The following function calculates the mean of the special "vector"
#created with the above function. However, it first checks to see
#if the mean has already been calculated. If so, it gets the mean
#from the cache and skips the computation. Otherwise, 
#it calculates the mean of the data and sets the value of the mean
#in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
