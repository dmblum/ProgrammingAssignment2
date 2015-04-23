
## The first function, `makeCacheMatrix` creates a list of 4 functions that do the following

## 1.  set the value of a matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

## a test is run to warn if matrix is not square, 

makeCacheMatrix <- function(x = matrix()) {
    if(dim(x)[1] != dim(x)[2]) warning("matrix not square and cannot be solved", immediate. = TRUE)
    m <- NULL
    set <- function(y) {
        if(dim(y)[1] != dim(y)[2]) warning("matrix not square and cannot be solved", immediate. = TRUE)
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

## The second function, cacheSolve, computes the inverse of matrix set using
## the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. 

## attempting to run cacheSolve on a non-square matrix will produce error:
## "(n x m) must be square"
## attempting to run cacheSolve on a square but non-invertible matrix will will produce error:
## "Lapack routine dgesv: system is exactly singular"

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
