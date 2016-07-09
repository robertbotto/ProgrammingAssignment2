## These functions allow the creation of a matrix with a cached inverse of 
## itself.  To use them, first call makeCacheMatrix to create an object 
## capable of cacheing.  Then pass the object to cacheSolve to compute the 
## inverse. 

## Takes a matrix and returns a list with the matrix and functions for 
## setting/getting the matrix, and setting/getting the matrix inverse.
## Note: matrix should be invertable.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Take the returned list from makeCacheMatrix and either returns the cached 
## inverse, or calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
