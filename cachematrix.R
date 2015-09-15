## The pair of functions below will work together to provide the ability to 
## cache the results of the "solve" process.
## Example usage:
##              m <- matrix(rnorm(100),10)
##              cacheMatrix <- makeCacheMatrix(m)
##              cacheSolve(cacheMatrix) ## this one will do the work.
##              cacheSolve(cacheMatrix) ## this one should come from cache.


## the makeCacheMatrix function will create the cacheMatrix object.
## the cacheMatrix object will include a private field "inverse" which holds the
## inverted value as cached. The object will also have a get and set function, 
## to set or retrieve the passed matrix value as well as a get and set for the 
## inverted and cached values.
## this object is of no use without the following function "cacheSolve"

makeCacheMatrix <- function(x = matrix()) {
        ## we store the solved/inverted matrix in here.
        ## this is essentially the cached value holder.
        inverse <- NULL
        
        ## set the the value of the matrix and clears the "inverse" variable.
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## get the value matrix
        get <- function() x
        
        ## set the inverse of the matrix.
        setinverse <- function(solved) inverse <<- solved
        
        ## get the 
        getinverse <- function() inverse
        
        ## return a named list with the values or functions for this object.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
        
}


## The following function calculates the inverse of the special "vector" created 
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverted matrix from the cache 
## and skips the computation. Otherwise, it inverts the supplied matrix  and 
## sets the value in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        ## if the inverse has already been created, print a message and return.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        else{
        ## if the inverse has not been created
        ## retrieve the matrix from x
        data <- x$get()
        
        ## create the inverse locally.
        inv <- solve(data, ...)
        
        ## set the inverse on x using the setinverse function.
        x$setinverse(inv)
        
        return(inv)
        }
        
}
