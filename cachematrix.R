# ############## Caching the Inverse of a Matrix #################
# 
# This example introduces the <<- operator which can be used to assign a value 
# to an object in an environment that is different from the current
# environment."The operators <<- and ->> are normally only used in functions,
# and cause a search to made through parent environments for an existing
# definition of the variable being assigned. If such a variable is found (and
# its binding is not locked) then its value is redefined, otherwise assignment
# takes place in the global environment." Below are two functions that are used
# to create a special object that stores a matrix and cache's its inverse.
# Caching can save calculation time.
# 
# SHORT COMMENT DESCRIBING THE makeCacheMatrix FUNCTION: The makeCacheMatrix 
# function creates a special "matrix" object that can cache its inverse. It uses
# a list of functions to: 1) set the value of the matrix; 2) get the value of 
# the matrix; 3) set the value of the inverse matrix; and, 4) get the value of 
# the inverse matrix. This list is passed to the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) { # makes a matrix to feed cacheSolve
        im <- NULL                          # sets im to nothing
        set <- function(y) {                # assignment of new matrix if need 
                x <<- y                     # assignment in global environment
                im <<- NULL
        }
        get <- function() x                 # gets the matrix
        setim <- function(solve) im <<- solve # assigns inverse matrix if wanted
        getim <- function() im              # gets the inverse matrix
        list(set = set, get = get,          # list passed to cacheSolve
             setim = setim,
             getim = getim)
}

# Two matrices for testing
a = matrix(c(4,7,2,6),nrow=2,ncol=2,byrow=T) # test matrix 'a'
b = matrix(c(0,2,2,0),nrow=2,ncol=2,byrow=T) # test matrix 'b'

# Call first function and give it the matirx.
data <- makeCacheMatrix(a) # assigns matrix
data <- makeCacheMatrix(b) # assigns matrix

# Commands for testing behaviors of the functions.
data$set(b) # assigns a new matrix, if needed.
data$get() # gives the matrix
data$setim(ib) # assigns inverse matrix to 'im' (e.g., ib, inverse b) if needed.
data$getim() # gives matrix; it's NULL unless changed by function 'setim'.

# SHORT COMMENT DESCRIBING THE cacheSolve FUNCTION: The cacheSolve function
# calculates the inverse of the matrix created with the makeCacheMatrix
# function. But it first checks whether the inverse matrix has already been
# calculated. If so, it gets the inverse matrix from the cache and skips the 
# computation. Otherwise, cacheSolve calculates the inverse matrix of the data
# and sets its value in the cache via the setim function.

cacheSolve <- function(x, ...) {
        im <- x$getim()                        # gets the inverse matrix
        if(!is.null(im)) {                     # checks if value in cache
                message("getting cached data") # if cached print message
                return(im)                     # gives cached value
        } else {                               # if cache=NULL, calclulate im
        data <- x$get()                        # gets the matrix
        im <- solve(data, ...)                 # calculates inverse matrix
        x$setim(im)                            # caches inverse matrix
        im}
}

# Calls cacheSolve function: returns the inverse matrix if cached or calculates it.
cacheSolve(data)  
