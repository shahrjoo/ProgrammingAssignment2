## Programming Assignments 2


## This function creates a special "matrix" object that can cache its inverse
## by using <<- operator

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  #initial the invers matrix
        
        #set function sets a matrix to an object created by makeCacheMatrix
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        # get function return the input matrix
        get <- function() x
        # setinvers function sets the invers matrix
        setInvers <- function(matrix) inv <<- matrix
        #getinvers function returns the invers matrix
        getInvers <- function() inv
        
        # return the list of functions set,get,setinvers,getinvers
        list(set = set, get = get,
             setInvers = setInvers,
             getInvers = getInvers)    
        
        
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInvers()  # get the invers matrix
        
        # if the inversed matrix is there, return that
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # if the iversion matrix is not there, get the matrix object
        data <- x$get()
        # solve the invers matrix
        m <- solve(data, ...)
        # set the inver matrix
        x$setInvers(m)
        # retrun solved matrix
        m
        
}
