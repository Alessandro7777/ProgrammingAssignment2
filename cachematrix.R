##  Write an R function that is able to cache potentially time-consuming computations. In this case the R function caches the inverse of a matrix. It will allow me to store it and not compute it repeatedly.


## makeCacheMatrix :  This function creates a special "matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	
	    inverse<- NULL
        
        set<- function (y) {
                x<<- y
                inverse<<- NULL}   ## function to set the value of the object.
        
        get<- function () x    ## function to extract the value from the object
        
        setinverse<- function(inverse) inverse <<- inverse  ## function to store the given value (as inverse) for future use.
        
        getinverse<- function()inverse  ## function to extract the inverse of matrix
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
	
	inverse<- x$getinverse()
	
	if (! is.null (inverse))  { message("getting cached data")
                                           return (inverse) 
                                                                           }     ## Return a matrix that is the inverse of 'x'  when computed previously

     data<- x$get()
    
     inverse<- solve (data,...)    ## return  the inverse of a matrix set previoulsy
     
      x$setinverse(inverse)
     inverse
       }
