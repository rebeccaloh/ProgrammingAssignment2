## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) x_inv <<- inv
        getinv <- function() x_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinv()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        m <- x$get()
        m_inv <- solve(m)
        x$setinv(m_inv)
        m_inv
}

