## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
