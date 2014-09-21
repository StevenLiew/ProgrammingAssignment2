#####################################################################
## Purpose      : Caching inverse of matrices
## Methods      : makeCacheMatrix, cacheSolve
## Module       : R Programming Module 2 Assignment 2
## Author       : Steven Liew
## 
#####################################################################

## Function     : makeCacheMatrix
## Input param  : Matrix
## Output param : List
## Purpose      : Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialising the inverse variable 
        i <- NULL
        
        ## Set the matrix 
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        
        ## Get the matrix 
        ## Simply return 'm'
        get <- function() m
        
        ## Set the inverse of the matrix
        setInverse <- function(inverse) i <<- inverse
        
        ## Get the inverse of the matrix
        ## Simply return 'i'
        getInverse <- function() i
        
        ## Return the list of the methods
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## Function     : cacheSolve
## Input param  : Inverse of the matrix in makeCacheMatrix()
## Output param : Matrix
## Purpose      : Computes the inverse of the matrix returned by 
##                makeCacheMatrix(). If the inverse has already
##                been calculated (and the matrix has not changed),
##                then retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Return the inverse if it is already set
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Get matrix from our object
        data <- x$get()
        
        ## Computes the inverse
        m <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setInverse(m)
        
        ## Return the matrix
        m
        
}
