## These functions operate very similarly to the example provided.
## One creates a list of functions to be tested and/or called by another function

## This function creates the "cache" by creating a list of functions that can be called
## to retrieve the value of a given matrix, and get OR set the value of its inverse.

makeCacheMatrix<-function(x=matrix()){
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
## This function calculates the inverse of the matrix present in the list
## returned by makeCacheMatrix(), but first checks to see if the inverse
## is already present in the list. If so, it returns the cached inverse.
## if not, it calculates the inverse, and sets its value

cacheSolve<-function(x,...){
    inv<-x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    matrix<-x$get()
    inv<-solve(matrix)
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
