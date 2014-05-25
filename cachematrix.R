## Functions to cache a matrix and get its inverse:
## - makeCacheMatrix takes in a matrix as an argument and caches it. It is also 
##   able to get and cache the inverse of a matrix. Please note it doesn't find 
##   the inverse of the matrix, just caches and gets the matrix and its inverse. 
## - cacheSolve gets a matrix, finds its inverse and caches the resulting inverse 
##   matrix.


## Data structure that gets a matrix and stores it and its inverse.
makeCacheMatrix <- function(x = matrix()) {

    # initialise the inverse matrix to NULL
    inversematrix <- NULL
    
    # set the matrix for the data structure
    set <- function(y) {
        x <<- y
        inversematrix <<- NULL
    }
    
    # get the matrix from the data structure
    get <- function() x
    
    # set the inverse matrix in the data structure, given an inverse matrix as input
    setinverse <- function(inverse) inversematrix <<- inverse
    
    # get the inverse matrix
    getinverse <- function() inversematrix
    
    # list of the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Find the inverse of a matrix
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    
    # check if there is already a cached inverse matrix for the input matrix.
    # If so, return the inverse matrix that has already been cached.
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # If the inverse matrix doesn't exist, then get the input matrix...
    data <- x$get()
    
    # find the inverse of the input matrix...
    inverse <- solve(data)

    # and cache the inverse of the input matrix, returning the matrix at the end
    # of the function.
    x$setinverse(inverse)
    inverse
}
