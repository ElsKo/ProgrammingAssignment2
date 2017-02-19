# DataScience - R Programming
# Programming Assignment 2: Lexical Scoping
#
# ############################################################################
# Assignment
#
#Write the following functions:
# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.
# 
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then cacheSolve should 
# retrieve the inverse from the cache.
# 
# 
############################################################################
#Explanation and example of use:
#
# 1. Create an example matrix
# 2. Function makeCacheMatrix is executed for the ExampleMatrix
# 3. The inverse matrix is computed.
# 4. The cached values are used.
#
# 1.ExampleMatrix <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# 2.myExampleMatrix_object <- makeCacheMatrix(ExampleMatrix)
# 3.cacheSolve(myExampleMatrix_object)
# 4.cacheSolve(myExampleMatrix_object)
#
############################################################################
##The function makeCacheMatrix creates an object that can cache its inverse.
##Input for this function is a matrix.
makeCacheMatrix <- function(x = matrix()) {   
    m <- NULL                   
    set <- function(y) {
        x <<- y                
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse= getinverse)
}

##The function cacheSolve computes the inverse the matrix. When the inverse
##is computed already, the cached values are used.
cacheSolve<- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}