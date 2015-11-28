## Matrix Inversion can be a costly computation and caching the Inverse can be helpful. If the contents of a matrix 
## are not changing, it makes sense to cache the value of the Inverse so that when we need it again, it can be looked
## up in the cache rather than recomputed.

## The pair of functions below are used to create a special "matrix" that stores the matrix and cache's its Inverse.

## MakeCacheMatrix creates a special "matrix" which is a list containing a function to - 
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the Inverse
## 4 - get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function calculates the Inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the Inverse has already been calculated. If so, it gets the Inverse 
## from the cache and skips the computation. Otherwise, it calculates the Inverse of the data and sets the 
## value of the Inverse in the cache via the setInverse() function.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

## End

