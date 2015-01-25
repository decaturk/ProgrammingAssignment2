## makeCacheMatrix creates a matrix object that can cache a matrix and its inverse. 
## cachesolve solves the inverse of the matrix, only computing when necessary (when the matrix has changed)
## otherwise it will retrieve the inverse from cache.
## 
## ex:  > y <- makeCacheMatrix(matrix(1:4,2,2))
##      > cacheSolve(y) 
 
## makeCacheMatrix creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL  ## set cached inverse to null on set 
    }
    get <- function() x
    setinv <- function(inv) m <<- inv ## set the inverse 
    getinv <- function() m 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the inverse of the makeCacheMatrix matrix 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    ## if not cached, compute inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}



