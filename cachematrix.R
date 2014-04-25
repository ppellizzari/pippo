## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is modelled after makeVector: it defines data
## and methods (set, get, setinverse and getinverse)
## the input is a matrix; the output is a list (enter object$get() to see the data)

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is modelled after cachemean
## if the inverse was already computed returns it from the cache;
## else it computes the inverse, cache it and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
