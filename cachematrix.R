## The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix: takes input of a matrix and creates a list of functions to cache inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        ## inv is inverse and initially set to NULL
        inv <- NULL
        ## set matrix data
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## return matrix data
        get <- function() x
        ## set the inverse in cache
        setinverse <- function(inverse) inv <<- inverse
        ## return the cached inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function assumes that the matrix is always invertible
## cacheSolve: computes inverse of a matrix made by makeCacheMatrix
## If inverse is already computed this function will return the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## get inverse from cache
        inv <- x$getinverse()
        # if inv is already cached, return the cache
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if inv is not cached, compute the inverse of the matrix and set in the cache
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}