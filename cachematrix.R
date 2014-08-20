## Below functions are used to cache/retrieve a matrix and its inverse.
## makeCacheMatrix() is responsible for caching/retrieving.
## cacheSolve() is responsible for calculating the inverse matrix.


## Returns a list of closures for setting/getting the matrix and its inverse.
makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    
    ## Reset the inverse matrix if any of the data in the matrix changes.
    set <- function(x) {
        if (!identical(mat, x)) {
            mat <<- x
            inv <<- NULL
        }
    }
    
    ## Get the matrix.
    get <- function() {
        mat
    }
    
    ## Set the inverse matrix as the given argument.
    setInverse <- function(y) {
        inv <<- y
    }
    
    ## Get the inverse matrix.
    getInverse <- function() {
        inv
    }
    
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse matrix.
cacheSolve <- function(matWrapper, ...) {
    inv <- matWrapper$getInverse()
    
    ## If the inverse matrix is not cached, the inverse matrix is 
    ## calculated and cached before it is returned.
    if (!is.null(inv)) {
        message("Cached data found.")
    } else {
        data <- matWrapper$get()
        inv <- solve(a = data, ...)
        matWrapper$setInverse(inv)
    }
    
    inv
}
