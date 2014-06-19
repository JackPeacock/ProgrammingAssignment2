## Put comments here that give an overall description of what your
## functions do

## These two functions provide functionality to be able to cache the results of 
## calculating the inverse of a matrix which can be a costly calculation.

## Write a short comment describing this function

## makeCacheMatrix(matrix) is a list which stores a matrix in the variable x.
## makeCacheMatrix(matrix)$get() returns the matrix
## makeCacheMatrix(matrix)$set(newmatrix) changes the value of the matrix, and 
## resets the value of the inverse
## makeCacheMatrix(matrix)$setinverse(inverse) caches the inverse of the matrix
## makeCacheMatrix(matrix)$getinverse() returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## This function checks to see if the inverse of the matrix has already been
## calculated and cached. If so, it returns it. If not, it calculates it,
## caches it using the setinverse function of makeCacheMatrix, and then returns
## it.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
