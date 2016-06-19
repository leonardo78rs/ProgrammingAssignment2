# R Programming - Assignment Week 3 
## 
## Leonardo Mendes de Oliveira 19/06/2016.
##
## Code example: 
##   abc<- makeCacheMatrix(cbind(c(1,1,2,1),c(1,3,5,1),c(3,3,3,1),c(1,2,3,1)))
##   cacheSolve(abc) # first = calc
##   cacheSolve(abc) # second = cache
## 

## makeCacheMatrix
#Creates a special matrix object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinve <- function(solve) i <<- solve
  getinve <- function() i
  list(set = set, get = get, setinve = setinve, getinve = getinve)
}



### cacheSolve 
## Return a matrix that is the inverse of 'x'
## This function calculates the inverse of the special matrix created with the above function. 
## The first time, computes the inverse matrix. After cached, no need to calculate.
## 
## The matrix provided must be reversible.
## Error in x$getinv : $ operator is invalid for atomic vectors


cacheSolve <- function(x, ...) { 
  i<- x$getinve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i<- solve(data, ...)
  x$setinve(i)
  i
  
}