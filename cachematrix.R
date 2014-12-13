## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
## Parameter: a matrix x
## Methods: set, get, setInverse, getInverse
makeCacheMatrix <- function(x = matrix()) {
        ## Inizialites the local variable
        inv <- NULL
        
        ## Changes the value of x globally
        ## Inizialites globally the variable inv
        set <- function(y) {
                x <<- y         
                inv <<- NULL
        }
        
        ## Returns the original matrix
        get <- function() x 
        
        ## Caches the inverse matrix 
        setInverse <- function(inverse) inv <<- inverse
        
        ## Returns the inverse matrix (cached)
        getInverse <- function() inv
        
        ## Methods that you can use over the object
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix
## Parameter: a matrix x
## Return: a inverse matrix of x
cacheSolve <- function(x) {
        
        ## Returns the cached matrix
        inv <- x$getInverse()
        
        ## Checks if we have an inverse cached matrix and gets the value (globally)
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if we do not have a cached matrix
        ## Takes the original matrix
        data <- x$get()
        ## Calculates the inverse of the matrix (only works with squuare matrix)
        inv <- solve(data)
        ## Sets the inverse matrix
        x$setInverse(inv)
        
        ## Returns a matrix that is the inverse of 'x'
        inv
}
