# In this exercise, we will create a pair of functions that cache the inverse of a matrix.

## The first function, below, creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inverse <- NULL 
set <- function(x) {
mtrx <<- x
inverse <<- NULL
}
 get <- function() return(mtrx)
    setinv <- function(inv) inverse <<- inv ##set the inverse matrix
    getinv <- function() return(inverse)##return the inverse matrix
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}


##The next function "cacheSolve" computes the inverse of the 
##special "matrix" returned by `makeCacheMatrix` above. 

##If the inverse has already been calculated
## (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(mtrx, ...) {
inverse <- mtrx$getinv()  ##inverse matrix from object x
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    
    data <- mtrx$get()
    inverse <- solve(data, ...)
    mtrx$setinv(inverse)
    return(inverse)## Return calculated inverse
}
