
## makeCacheMatrix function receives as param a matrix from n x n elements (square matrix) , that contains a list of functions
## Each function must be called from the cacheSolve function
## Once a matrix is created with the command    myMatrix <- makeCacheMatrix(amatrix) 
## With the previous call, you can after run the command  myMatrix$get() or execute the other functions in this same $ format

## Once the makeCacheMatrix creates the matrix with a list of operations, the cacheSolve function will run the inverse of a matrix
## and will store the result in cache. The second time the function is called , if the matrix didnt change the  result will be
## returned from cache.


makeCacheMatrix <- function(x = matrix()) {
  msolve <- NULL
  set <- function(y) {
    x <<- y
    msolve <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) msolve <<- solve
  getsolve <- function() msolve
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
  msolve <- x$getsolve()
  if(!is.null(s)) {
    message("Getting cached data")
    return(msolve)
  }
  data <- x$get()
  msolve <- solve(data, ...)
  x$setsolve(msolve)
  msolve
}