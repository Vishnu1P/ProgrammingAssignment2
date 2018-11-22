## Cache the Inverse of a Matrix

## makeCacheMatrix -   Creates a special matrix object that 
##                     can cache it's inverse
##                     (or another way to describe ) 
##                     Create a list of functions
##                     like set/get/setinverse/getinverse 
##                     of the arguement matrix and it's inverse
                  

## set - Is for setting the matrix
## get - Get the value of the Matrix
## getinverse - Get the inverse of the matrix
## setinverse - Set the inverse to be the supplied arguement inverse.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <-function(y){
        x<<-y
        m<<-NULL
      
    }
    get <-function()x
    setinverse <- function (inversemat) m <<- inversemat
    getinverse <- function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## cachesolve - Computes the inverse of the special matrix returned by 
##              makeCacheMatrix above. If the inverse has already been 
##              calculated (and the matrix has not changed), then the
##              cachesolve retreive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  
  
}
