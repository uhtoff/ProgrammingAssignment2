## This pair of functions are designed to store a single inverted matrix to prevent
## having to invert it repeatedly.
## First the makeCacheMatrix function is called with a matrix and assigned to a variable
## Then this variable is passed to the the cacheSolve function to get the inverse, if
## it is passed again without resetting the matrix then the inverse will be retrieved
## from the cache

## This function accepts a matrix and stores it in a closure so it can be retrieved
## by using the get method.  It can also store the inverse of that matrix and return
## that using the setinverse and get inverse methods respectively

makeCacheMatrix <- function(x = matrix()) {
	## Clear the inverse on initiation
        i  <- NULL
	## Set method which replaces the stored matrix and clears the inverse
        set  <- function(y){
                x <<- y
                i <<- NULL 
        }
	## Get method which stores the inputted matrix in a closure
        get  <- function() x
	## Setinverse method storing the inverse as i (note <<- scope selector)
        setinverse  <- function(inverse) i  <<- inverse
	## Getinverse method to retrieve the inverse
        getinverse  <- function() i
	## List so that the methods are returned accessible using $
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## This function accepts the matrix created by makeCacheMatrix, checks to see if an
## inverse is set, if it is then that is returned along with a message saying it is 
## from the cache.  Otherwise, the inverse is computed and stored in the cache function

cacheSolve <- function(x, ...) {
	## Attempt to retrieve the inverse
        i  <- x$getinverse()
        if (!is.null(i)){
	## Return the retrieved value
                message("Getting cached data")
                return(i)
        }
	## Otherwise get the original matrix
        data  <- x$get()
	## Inverse it
        i  <- solve(data, ...)
	## and store it in the cache function
        x$setinverse(i)
        i
}

