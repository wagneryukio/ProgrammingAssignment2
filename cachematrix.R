## Here we have two functions. 
## The makeCacheMatrix function creates a list of functions for getting
## and setting both the matrix that will be inverted and the resulting 
## inverted matrix. 
## The cacheSolve function returns a matrix which is the inverse of the
## matrix passed inside it's argument. It's argument must a "CacheMatrix" 
## created by the makeCacheMatrix function.

## This function receives a matrix as argument and creates get and set 
## functions to read/write to this matrix. It also creates get and set 
## functions to read/write to the cache which keeps the inverted matrix
## of the matrix set inside of it. If a new matrix is set, then the 
## cached inverted matrix is set to NULL.
## This function returns a list with the four functions in it. 

makeCacheMatrix <- function(x = matrix()) {
    # initializes the inverse matrix as NULL
    inverse <- NULL
    
    # this function sets a new matrix in the variable x and initializes
    # the cache back to NULL. Without doing this the cacheSolve function
    # would always return the cached inverse even if a new matrix was set.
    # Note the use of the <<- attribution to set the value of the x and 
    # inverse variables which are in the parent environment.
    set <- function(newmatrix) {
        x <<- newmatrix
        inverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(i) {
        inverse <<- i
    }
    
    getinverse <- function() {
        inverse
    }
    
    # Returns a list containing the functions which will manipulate 
    # the data.
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}


## This function receives the "CacheMatrix" created by the first function 
## as argument and looks for the cached inverted matrix. If it exists
## it will return it. Otherwise it will invert the matrix using the solve()
## function and will then cache the result for future use.
## It returns the inverted matrix

cacheSolve <- function(x, ...) {
    # Tries to fetch and return the cached inverse matrix
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Returning the cached inverse")
        # Returns the cached inverse matrix
        return(i)
    }
    
    # The cached inverse matrix was NULL, which means that a new matrix
    # was set inside x and the inverse must be calculated.
    matrix <- x$get()
    i <- solve(matrix, ...)
    # Updates the inverse matrix cache
    x$setinverse(i)
    
    # Returns the newly calculated inverse matrix
    i
}
