## A pair of functions to cache a matrix object's inverse.
## This saves processing time.

# The first function, `makeCacheMatrix` creates a special "matrix" object
# that can cache its inverse.

# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve works in tandum with the makeCacheMatrix function above.
# cacheSolve will get value for the inverse of the special matrix from   
# from makeCacheMatrix.  If the cached value is not available, it
# will generate one.

# Note: the matrix must be invertible for this function to work.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
