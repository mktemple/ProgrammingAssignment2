## cachematrix.R code by mktemple

## This makeCacheMatrix function will:
## 1- get a matrix as an input
## 2- will set the value of the matrix
## 3- get the value of the matrix
## 4- set the inverse of the matrix
## 5- get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      
    invmatrix <- NULL
    
  
    setmatrix <- function(y){
      
          x<<-y
          invmatrix<<- NULL
    }  
    
    getmatrix<-function() x
    setinverse <- function(inverse) invmatrix<<-inverse
    getinverse <-function() invmatrix
    
  list(setmatrix=setmatrix,getmatrix=getmatrix,setinverse=setinverse,
       getinverse=getinverse)
    
}


## uses the output of the makeCacheMarix function as an input.
## checks the inversion matrix from the functin to see if it has a value.
## if the inversion matrix is empty it gets the origial matrix and
## sets the inverse matrix using the solve function.
## if the inversion matrix is not empyt it diplays the mesage 
## "Getting Cached Inverted Matrix" and returns the cached object.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    invmatrix <- x$getinverse()
    if(!is.null(invmatrix)) {
      message("Getting Cached Inverted Matrix")
      return(invmatrix)
    }
    data <- x$getmatrix()
    invmatrix <- solve(data, ...)
    x$setinverse(invmatrix)
    return(invmatrix)

}
