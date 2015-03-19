## These functions operate very similarly to the example provided.
## One creates a list of functions to be tested and/or called by another function

## This function creates the "cache" by creating a list of functions that can be called
## to retrieve the value of a given matrix, and get OR set the value of its inverse.

makeCacheMatrix<-function(x=matrix()){
    inv <- NULL                   #inital NULL value for the inverse
    set <- function(y) {          #function to set the value of the MATRIX
        x <<- y
        inv <<- NULL
    }
    get <- function() x           #function to retreive the value of the MATRIX
    setinv <- function(inverse) inv <<- inverse  #function to set the value of the INVERSE
    getinv <- function() inv      #function to retrieve the value of the INVERSE
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
## This function calculates the inverse of the matrix present in the list
## returned by makeCacheMatrix(), but first checks to see if the inverse
## is already present in the list. If so, it returns the cached inverse.
## if not, it calculates the inverse, and sets its value

cacheSolve<-function(x,...){
    inv<-x$getinv()         #retreives value of inverse
    if(!is.null(inv)){      #checks to see if value is not NULL
        message("getting cached data")
        return(inv)         #returns inverse matrix
    }
    matrix<-x$get()         #gets value of matrix
    inv<-solve(matrix)      #solves for inverse
    x$setinv(inv)           #stores inverse in cache list
    inv                     #returns inverse matrix
        ## Return a matrix that is the inverse of 'x'
}
