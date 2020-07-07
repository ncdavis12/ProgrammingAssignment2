## Inverses Matrixes via caching rather than through computation

## creates a matrix object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(z) {
                x <<- z
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Returns the inverse Matrix

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("retrieving cached Matrix")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}
