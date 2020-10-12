# Matrix inverse caching functions

# Create a matrix object with ability to cache value for inverse. 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { 
        x <<- y 
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(y) { 
        inv <<- y
    }
    getinverse <- function() inv
    list(set = set, get = get,
         getinverse = getinverse,
         setinverse = setinverse)
}

# Takes a matrix object and checks if its inverse has been cached. 
# If not, calculate the inverse and cache.
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv) 
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
