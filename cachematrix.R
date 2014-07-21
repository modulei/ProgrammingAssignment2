## Below are two functions that are used to create a special object that stores
## a square matrix and cache's its inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                        # Check if matrix has changed
                        if (identical(x,y)) return
                        x <<- y
                        m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Assign any existing inverse to m
        m <- x$getinv()
        ## Check if data already cached and if so, return cached data in m
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Pull in the value of the special matrix
        data <- x$get()
        ## Calculate the inverse of data and assign to m
        m <- solve(data, ...)
        ## Set the solution to x
        x$setinv(m)
        ## Return a matrix that is the inverse of 'x'
        m
}