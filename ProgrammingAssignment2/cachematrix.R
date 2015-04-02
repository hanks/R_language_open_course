## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    reverse_matrix <- NULL
    
    set <- function(y) {
        x <<- y
        reversed_matrix <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setReverse <- function(r) {
        reversed_matrix <<- r
    }
    
    getReverse <- function() {
        reversed_matrix
    }
    
    list(set=set,
         get=get,
         setReverse=setReverse,
         getReverse=getReverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        reversed_matrix <- x$getReverse()
        
        if (!is.null(reversed_matrix)) {
            return (reversed_matrix)
        }
        
        data <- x$get()
        
        m <- solve(data)
        
        x$setReverse(m)
        
        m
}
