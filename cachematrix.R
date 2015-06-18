## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than computing 
## it repeatedly.  The following pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## x is a square invertible matrix
        ## return is a list containing functions to:
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ## result is used as the input to cacheSolve()
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, 
                setinv=setinv, 
                getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x is the output of makeCacheMatrix()
        ## return is the inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
        ## if the inverse has already been calculated
        if (!is.null(inv)){
                # get the result from the cache and skip the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # if not cached, calculate the inverse 
        data = x$get()
        inv = solve(data, ...)
        
        # sets the value of the inverse in the cache
        x$setinv(inv)
        
        return(inv)
}
