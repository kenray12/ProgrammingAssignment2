## makeCacheMatrix takes an invertible matrix as its input and provides
## four subfunctions that allow caching of intermediate results:
## set, get, setsolve, and getsolve
## (no error checking of the input matrix is done)

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

## cacheSolve takes the matrix 'prepped' by 'makeCacheMatrix' and either:
## - solves it if it has not been solved yet' or
## - returns the cached solution if it has been solved already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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