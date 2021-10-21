## Put comments here that give an overall description of what your
## functions do

## function caches supplied matrix & inverse matrix values and provides an interface to access them.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<-inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv,getinv = getinv)

}


## function attempts to retrieve a cached inverse of the supplied matrix, if not available it proceeds to inverse the matrix and cache the result via the makechaematrix function.

cacheSolve <- function(x, ...) {
       
        invmat <- x$getinv()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data)
        x$setinv(invmat)
        invmat

}
