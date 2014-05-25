## FILE: cachematrix.R
## AUTHOR: Rei Shinozuka shino@panix.com
## 

## makeCacheMatrix creates an object which stores matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                 ## inv is cached inverted 
    
    set <- function(y) {        ## set() sets source matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x         ## get() obtains source matrix
    setinv <- function(invmat) inv <<- invmat ## setinv() set the cached inverted matrix
    getinv <- function() inv    ## getinv() obtains cached inverted matrix
    
    list(set = set, get = get,  ## store accessor and mutator methods
         setinv = setinv,
         getinv = getinv)
}


## cachesolve calculates inverse of input matrix, using cached data if available
##            using the cache save computational time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()           ## try to get cached data
    if(!is.null(inv)) {         ## if set, then use
        message("getting cached data")
        return(inv)             ## send back
    }
    ## at this point, inv not set, so calculate and set it
    data <- x$get()             ## get matrix
    inv <- solve(data, ...)     ## perform inversion
    x$setinv(inv)               ## cache the result
    inv                         ## return
}

## testSolve is a testing rig intended to check the correct operation of makeCacheMatrix() and cacheSolve() functions
##           it is not part of the assignment
testSolve <- function() {
                                ## create a diagonal test matrix
    size<-5
    testMatrix <- matrix(0, nrow=size,ncol=size)
    for (i in 1:size) {
        testMatrix[i,i]<-i
    }
    print("Input Test Matrix")
    print(testMatrix)           ## print out test matrix
    testCacheMatrix<-makeCacheMatrix(testMatrix)
                                ## print inverted matrix, this should actually calculate inverted matrix
    print("Test Matrix Inverted")
    print(cacheSolve(testCacheMatrix))
                                ## print inverted matrix again, this time should use cached inverted matrix
    print("Test Matrix Inverted (Cached)")
    print(cacheSolve(testCacheMatrix))
                                ## create a new matrix
    size<-4
    testMatrix <- matrix(0, nrow=size,ncol=size)
    for (i in 1:size) {
        testMatrix[i,i]<-i*2
    }
    print("Second Test Matrix")
    print(testMatrix)           ## print out test matrix again
    print("Second Test Matrix Inverted (Recalculated)")
                                ## setting input matrix, which will invalidate cache
    testCacheMatrix<-makeCacheMatrix(testMatrix)
    ## print inverted matrix, this should recalculate inverted matrix
    print(cacheSolve(testCacheMatrix))
}