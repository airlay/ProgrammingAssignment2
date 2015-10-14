## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix puts matrix x in and output
## inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL ## set s to NULL.  It will be used to save the solved 
            ##matrix
  
  set <- function(y){
    x <<- y   ## set matrix
    s <<- NULL ## leave the solved matrix NULL
  }
  
  get <- function() x  ## return the matrix
  
  setInverse <- function(solve) s <<- solve 
          ## inverse the matrix 
  
  getInverse <- function() s ## return inversed matrix
  list(set=set, get=get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function take x as matrix and try to 
## solve the inverse matrix of the original one
## it applies getInverse() first. Check if 
## the return value is not null 
## if it's null, the actual matrix will be solve
## by calling solve matrix and set the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getInverse()   ## assigned inversed matrix
  
  ## check if s is NULL or not
  if(!is.null(s)){
    message("getting cashed matrix")
    return (s)
  }
  mtx <- x$get()
  s <- solve(mtx, ...)
  x$setInverse(s)
  s
}
