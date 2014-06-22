## This file implements "Caching the Inverse of a Matrix" assignemnt


makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## This function, makeCacheMatrix creates a special "matrix", which is a list containing a function to:
## set: set the value of the matrix
## get: get the value of the matrix
## setinv: set the value of the Inverse of the matrix
## getinv: get the value of the Inverse of the matrix    
## x is an invertible matrix     
    
    
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinv <- function(solve) invm <<- solve
    getinv <- function() invm
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'. 
    ## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the 
    ## cache and skips the computation. 
    ## Otherwise, it calculates the inverse of the matrix and sets the value in the cache via the setinv function 
    
    ## x is an invertible matrix   
   

    invm <- x$getinv()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinv(invm)
    invm
}
