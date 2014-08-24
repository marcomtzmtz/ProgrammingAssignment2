## These two functions give the inverse of a matrix: first check if this value has been obtained before; if so, this function get the value from the cache; if not the inverse of the matrix is calculated, and also stored.

## Basically, this first function stores a list of original and inverted matrices to be used in the next function. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function will see if the inverse of the matrix has been stored; if not it will calculate, store it and return it.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
