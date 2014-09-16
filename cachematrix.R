## The functions below provide a way to prevent that the inverse of a matrix variable is calulated twice
## Since matrix inversion is computationally expensive for large matrices this can save CPU time


## makeCacheMatrix makes a matrix object that is actually a list
## containing four functions through which the matrix is accessible
## list names:
## $set(x): sets the matrix to the value given by x
## $get(): returns the current value of the matrix or NULL if not yet set
## $setinverse(x): sets the inverse of the matrix as the matrix given by x
## $getinverse(): returns the current value of the inverse of the matrix or NULL if not yet set

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) m <<- mean
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## makeCacheMatrix calculates the inverse of an matrix object created by makeCacheMatrix
## if the inverse was already calculated earlier than it returns the cached result 

cacheSolve <- function(x = matrix()) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
