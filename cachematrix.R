## Makes cacheable matrix that caches it's inverse matrix.
## The cache is invalidated only if underlying matrix is changed
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y  
        cachedInverse <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inverse) {
        cachedInverse <<- inverse
    }
    getInverse <- function() {
        cachedInverse
    }
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}


## Computes inverse of given "cache matrix".
## The result is cached until the original matrix is changed via x.set(newMatrix) method
cacheSolve <- function(x, ...) {
    xInverse <- x$getInverse()
    if(!is.null(xInverse)) {
        message("getting cached inverse")
        return(xInverse)
    }
    
    xInverse <- solve(x$get())
    x$setInverse(xInverse)
    xInverse
}
