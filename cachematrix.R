##functions cache the inverse of a matrix

## creates a matrix object that can cache its inverse

makeCacheMatrix<- function(x = matrix()){
     m<-NULL
## set the matrix
     set<- function(matrix){
          x<<- matrix
          m<<- NULL
     }
## get the matrix
     get<- function()x
## set the inverse of the matrix
     set_inverse<- function(inverse) m<<- inverse
## get the inverseof matrix
     get_inverse<- function()m
## return a list of above methods
     list(set= set, get= get, set_inverse= set_inverse,get_inverse =get_inverse)
}
 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve<- function (x, ...){
	m<-x$get_inverse()

## return the inverse if its already set
	if(!is.null(m)){
		message("getting cache matrix")
		return(m)
	}
## Get the matrix from our object
	data<- x$get_inverse()

## Calculating the inverse using matrix multiplication
      m <- solve(data)
      x$set_inverse(m)
      m
}    