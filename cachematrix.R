## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- mean
    getsolve <- function() m
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## Write a short comment describing this function

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

t <- matrix(c(1,1,1,1), nrow = 2, ncol = 2)
print(t)
bubba <- makeCacheMatrix()
bubba$set(t)
bubba$get()
cacheSolve(bubba)

# zzz <- makeCacheMatrix$set(matrix(c(2, 1, 1, -5, -3, 0, 1, 1, -1), nrow = 3, ncol = 3))

# print(zzz.get())

# cacheSolve(tmp)