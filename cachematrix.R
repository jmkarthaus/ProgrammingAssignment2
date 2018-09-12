## Programming Assignment 2
## The pair of the following functions can cache the inverse of a matrix.
## This will reduce the time needed for computation in case the calculation
## has been performed before.

## The first function creates the cached Matrix and its Inverse in a seperate environment.

makeCacheMatrix <- function(x = matrix()) {
        # object for cached inverse of matrix
        inv <- NULL
        ## get and set function for matrix
        get <- function() x
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get and set function for matrix inverse
        getinv <- function() inv
        setinv <- function(inverse) inv <<- inverse
        ## return list of functions for matrix
        list(get=get, set=set, getinv=getinv, setinv=setinv)
}

## The second function retrieves the inverse from the cache if the inverse has been calculated before.
## If it has not been calculated, the inverse is calculated in loaded in the catch.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        # if inverse has been calculated before, return cached matrix inverse
        if (!is.null(inv)) {
                message("retrieving cached inverse")
                inv
        }
        # calculate inverse of matrix 
        m <- x$get()
        inv <- solve(m, ...)
        # cache inverse
        x$setinv(inv)
        # return inverse of matrix
        inv
}

## Example 1
m<-matrix(c(1,0,0,1),2,2)
m_new<-makeCacheMatrix(m)
m
cacheSolve(m_new)

##Example 2
m2<-matrix(c(4,2,7,6),2,2)
m2_new<-makeCacheMatrix(m2)
m2
cacheSolve(m2_new)
