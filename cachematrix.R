## The following functions cache matrix inversion to significantly reduce the computation time.

## Example for a matrix x:
## cacheable.matrix <- makeCacheMatrix(x)

## The first call from cacheSolve calculates the inverse:
## inverse <- cacheSolve(cacheable.matrix)

## The following iterations will skip the calculation and return the value stored in cache.
## inverse.2 <- cacheSolve(cacheable.matrix)


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
            inv.stored <- NULL
            set.matrix <- function (new.matrix){
                    x <<- new.matrix
                    inv.stored <<- NULL
            }
            get.matrix <- function() x
            set.inverse <- function(inverse) inv.stored <<- inverse
            get.inverse <- function() inv.stored
            list(set.matrix = set.matrix, get.matrix = get.matrix,
                 set.inverse = set.inverse, get.inverse = get.inverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
            inv.stored <- x$get.inverse()
            if(!is.null(inv.stored)){
                    message("getting cached inverse")
                    return(inv.stored)
            }
            matrix <- x$get.matrix()
            inverse <- solve(matrix, ...)
            x$set.inverse(inverse)
            inverse
}
