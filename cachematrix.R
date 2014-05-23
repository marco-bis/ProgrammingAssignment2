## These functions cache the inverse of a matrix.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) m <<- mean
        getinverse <- function() m
        mv<-list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- NULL
        mat<<-x
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        mv<-list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        m <- mv$getinverse()
        if((!is.null(m))&&(x==mat)) {
                message("getting cached data")
                return(m)
        }
        data <- mv$get()
	  data
        m <- solve(data, ...)
        mv$setinverse(m)
        print(m)
        print(x)
}
