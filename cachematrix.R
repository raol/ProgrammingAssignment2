## Put comments here that give an overall description of what your
## functions do
## As caclulating inverse matrix is time consuming operations
## it's better to calculate it ones for the given matrix
## and store it in the cache. When matrix is changed (not its values)
## but the matrix object itself cache is flushed.
## So these to functions are designed to create special cache object
## that acts as a wrapper of a matrix, and when inverse matrix
## is needed to get it from the cache if it's there, or calculate 
## it and store in the cache for the further usage


## This function returns wrapper object above the actual matrix
## and adds additional functions to get its inverse matrix
## 
## Wrapper object methods (functions)
## set - function to store the given matrix
## get - get stored matrix
## setinverse - cache inverse matrix of the stored one
## getinverse - get cached inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        
        ## when setting a new matrix we have to
        ## clean up cache because inverse matrix
        ## is no longer valid
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) m <<- inverse
    
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function is used to calculate inverse matrix of the
## given matrix
## If the value has already been calculated and stored in cache
## it's taken from the cache. If the cache is empty, inverse matrix 
## is calculated and stored in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    
    inv
}
