## The purpose of this activity is to illustrate how
## both the  makeCacheMatrix" and "cacheSolve" functions 
## can  be used to cache the inverse of a matrix

## The makeCacheMatrix function will create matrix object that can 
## cache its inverse for the input as an invertible square matrix

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## In this section We wiil see how cacheSolvefunction will be used to computes the inverse of the 
## matrix object returned from the makeCacheMatrix above. 


cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()