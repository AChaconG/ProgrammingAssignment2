## Function that receives a matrix and creates a special "matrix", which is really a list, functions to:
# Set the value of the matrix
# Get the value of the matrix
# Set the value of the inverse of the matrix
# Get the value of the inverse of the matrix


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




## Function that calculates the inverse of the special "matrix" created with the above function. 
# It first checks to see if the inverse has already been calculated.
# If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache
# via the setsolve function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
}


