makeVector <- function(x = numeric(),mv) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        mv<-list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
print(mv)
}

cachemean <- function(x, ...) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        mv<-list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)

        m <- mv$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- mv$get()
	  data
        m <- mean(data, ...)
        mv$setmean(m)
        print(m)
        print(x)
}

