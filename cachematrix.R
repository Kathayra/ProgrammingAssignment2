## Functions cache the inverse of a matrix.
##June 2016


## This function creates a special "matrix" object
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #set the value of the matrix
  inversed_matrix <-NULL
  set <- function(y){
    x <<- y
    inversed_matrix <<- NULL
  }
  #get the value of the matrix
  get <- function () x
  
  setinverse <- function(solve) inversed_matrix <<- solve
  getinverse <-function() inversed_matrix
  list(set=set, get=get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special
##"matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversed_matrix <- x$getinverse()
  if(!is.null(inversed_matrix)){
    message("getting the cached data")
    return(inversed_matrix)
  }
  #invert matrix
  data <- x$get()
  inversed_matrix <- solve(data,...)
  x$setinverse(inversed_matrix)
  inversed_matrix
}
