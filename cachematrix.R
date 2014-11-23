## The two functions defined in this file are used to create a special object that 
## stores a matrix and caches its inverse. The function assumes that that the Matrix
## passed in is invertible. The inverse of the Matrix has been calculated using Solve
## function available in R

## This function takes in a matrix. The function declares a variable to store the inverse 
## of the passed matrix. The inverse variable can be accessed using geInverse and setInverse
## functions. The function returns a list of methods for setting and getting the matrix and 
## its iverse
makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setInverse <- function(inverse) inv <<- inverse
            getInverse <- function() inv
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)	
}

## This function takes in the special matrix created in the previous function as a parameter
## and returns the Inverse of the matrix. If the inverse is already stored in cache, it will return
##that else it will calculate the inverse using the solve function.

cacheInverse <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'	
            inv <- x$getInverse()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setInverse(inv)
            inv
}

##Following is an example of how to use these functions. Type in the following commands to test the funtion
##1.  a <- matrix(c(4,5,3,1,2,6,7,5,9),3)
##2.  b <- makeCacheMatrix(a)
##3.  c<- cacheSolve(b)
##4.  c


