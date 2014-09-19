## This function returns list of functions which can set and get matrix object also its inverse.
## Cache is maintained for matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv_mat) x_inv <<- inv_mat
        getinv <- function() x_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function checks whether inverse of matrix exist or not
## If inverse exist then it will return precomputed inverse
## If inverse of matrix is null then it will compute inverse and store back in cache for further calculations

cacheSolve <- function(x, ...) {
        x_inv <- x$getinv()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data, ...)
        x$setinv(x_inv)
        ## Return a matrix that is the inverse of 'x'
        x_inv
}
