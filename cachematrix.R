##functions cache the inverse of a matrix

## creates a matrix object that can cache its inverse

makeCacheMatrix<- function(m = matrix()){
     inv <- NULL
## set the matrix
     set<- function(matrix){
          m <<- matrix
          inv <<- NULL
     }
## get the matrix
     get <- function()m
## set the inverse of the matrix
     setinverse <- function(inverse) inv <<- inverse
## get the inverse of matrix
     getinverse <- function()inv
## return a list of above methods
     list(set = set, get = get, 
	  setinverse = setinverse,
	  getinverse = getinverse)
}
 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function (x, ...){
 m <- x$getinverse()
## return the inverse if its already set
	if(!is.null(m)){
		message("getting cached matrix")
		return(m)
	}
## Get the matrix from our object
	data <- x$get()

## Calculating the inverse using matrix multiplication
      m <- solve(data,...)
      x$setinverse(m)
      m
}    