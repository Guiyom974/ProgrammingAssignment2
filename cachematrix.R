## These functions stores the invert of matrix and makes possible to reuse the cached inverse

## Creates special matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvert <- function(invert) m <<- invert
    getinvert <- function() m
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## Inverts a matrix or retrieves it if already calculated

cacheSolve <- function(x, ...) {
    m <- x$getinvert()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvert(m)
    m
}

