## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
## Parameter: a matrix x
## Methods: set, get, setInverse, getInverse
makeCacheMatrix <- function(x = matrix()) {
        ## Inizialites the variable
        inv <- NULL
        
        ## superoperator caches in the global enviroment
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
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix
## Parameter: a matrix x
## Return: a inverse matrix of x
cacheSolve <- function(x) {
        
        ## Returns the cached matrix
        inv <- x$getInverse()
        
        ## Checks if we have a cached matrix and gets the value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if we do not have a cached matrix
        ## Takes the original matrix
        data <- x$get()
        ## Calculates the inverse of the matrix
        inv <- solve(data)
        ## Sets the inverse matrix
        x$setInverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
}
