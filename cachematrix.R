## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##@makeCacheMatrix: creates a special "matrix", which is really a list containing a function to 
##@set:set the value of the matrix
##@get:get the value of the matrix
##@setsolve: set the solve of the matrix
##@getsolve: get the solve of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## Write a short comment describing this function
## @cacheSolve: calculates the solve of the special "matrix" created with the above function
## If the solve is computed before, then it will get the solve from the cache
## If the solve is NOT computed before, then it will compute and cache it
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    ## Return a matrix that is the inverse of 'x'
}


# Example
# t <- matrix(c(3,1,0,2,1,1,1,-1,2), nrow = 3, ncol = 3)
# p <- matrix(c(6,-2,-7,4), nrow = 2, ncol = 2)
# bubba <- makeCacheMatrix()
# bubba$set(t)
# bubba$get()
# cacheSolve(bubba)
# cacheSolve(bubba)
# bubba$set(p)
# bubba$get()
# cacheSolve(bubba)
# cacheSolve(bubba)
