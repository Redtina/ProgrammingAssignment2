makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache
## its inverse
## Input = a square matrix that can be inverted
## Returns the following elements in a list
##      set the matrix
##      get the matrix
##      set the inverse of the matrix
##      get the inverse of the matrix
## The inputs above are used as input to cacheSolve()
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix.  If the inverse has already been calculated
## then the function will retrieve the inverse from cache.
## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
## If the inverse has already been calculated...        
        if(!is.null(inv)) {
                ## get it from cache
                message("getting cached data")
                return(inv)
        }
        ## otherwish calculate the inverse here
        data <- x$get()
        inv <- solve(data, ...)
        ## set the value of the inverse in the cache via the setinv function
        x$setinv(inv)
        return(inv)
}
