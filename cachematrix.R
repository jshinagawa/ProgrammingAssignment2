## a pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        get <- function() x
        getsol <- function() s 
        setsol <- function(sol) s <<- sol
        list(get = get, getsol = getsol, setsol = setsol)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getsol()
        print ("s=")
        s
        if (!is.null(s)) {
                message("getting chached data")
                return(s)
        }
        data <- x$get()
        print ("data=")
        data
        s <- solve(data, ...)
        x$setsol(s)
        print ("solve=")
        x$getsol()
}

