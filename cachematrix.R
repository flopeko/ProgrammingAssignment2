## Programming Assignment 2: Lexical Scoping. Caching the inverse of a 
## matrix: functions that calculate the inverse of a  matrix, in case 
## it's not calculated previously. 

## makeCacheMatrix creates a list that sets and gets the values of a matrix 
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
        # initialize the inverse
        i<<- NULL 
        # function to set the value of the matrix
        set<- function(y){ 
                y<<- x
        }
        # function to get the value of the matrix
        get <- function() x 
        # function to set the function that calculates the inverse of a matrix
        setinverse<- function(solve) i<<- solve 
        # function to get the value of the inverse
        getinverse<- function() i 
        # list containing the previous values
        list(set= set, get= get, setinverse= setinverse, getinverse= getinverse)
}


## cacheSolve calculates the inverse of the matrix set in the makeCacheMatrix
## function. It first checks if the inverse has already been calculated. In
## that case, gets the inverse from the cache.

cacheSolve <- function(x, ...) {
        # assign the value of the inverse searching in the getinverse() function
        # calculated above
        mi<<- x$getinverse()
        # checking if the value already exists
        if(!is.null(mi)) {
                message("getting cached inverse")
                return(mi) # return the previously calculated inverse
        }
        # composing the instruction to calculate the inverse
        m<- x$get()
        mi <- solve(m, ...)
        # setting the new value of the inverse
        x$setinverse(mi)
        # returns the inverse
        mi
}