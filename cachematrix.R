## Programming Assignments 2
## this function 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInvers <- function(matrix) inv <<- matrix
        getInvers <- function() inv
        list(set = set, get = get,
             setInvers = setInvers,
             getInvers = getInvers)    
        
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvers()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvers(m)
        m
        
}
