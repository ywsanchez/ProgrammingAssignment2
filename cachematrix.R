## Assignment 2

## This function creates a special "matrix" object that can cache its inverse.
# 1. set the value of the matrix	
# 2. get the value of the matrix	
# 3. set the value of inverse of the matrix	
# 4. get the value of inverse of the matrix	
makeCacheMatrix <- function(x = matrix()) {
        #inv stores cached inversed matrix
        inv <- NULL
        #set the value of the matrix
        set <- function(y) {	
                x <<- y	
                inv <<- NULL	
        }
        #get the value of the matrix
        get <- function() x
        #set value of inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse	
        #get value of inverse of the matrix
        getinverse <- function() inv
        #return matrix with new functions
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)	
}	

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.	

cacheSolve <- function(x, ...) {
        # store the cached inverse matrix
        inv <- x$getinverse()	
        
        # if statement to return inverse if already calculated
        if(!is.null(inv)) {	
                message("getting cached data")	
                return(inv)	
        }
        # calcuate the inverse if it is not already calculated
        data <- x$get()	
        inv <- solve(data, ...)	
        x$setinverse(inv)	
        inv	
}	