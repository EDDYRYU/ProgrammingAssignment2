## Put comments here that give an overall description of what your
## functions do

## This function will return inverse matrix as follows.
## e.g.
## For a matrix called "A",
## B <- makeCacheMatrix(A)
## cacheSolve(B) will return an inverse matrix of A,
## onlyif A is a square matrix wich has non-zero determinant.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #e.g. Y <- makeCacheMatrix(matrix)
    m <- NULL #assign initial m value into NULL whenever makeCacheMatrix function is used.
    set <- function(y) x <<- y
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, #list of function
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    # After running cacheSolve and before using makeCacheMatrix,
    # assign m cached data, but initially NULL as defined as above function
    if(!is.null(m)){
        message("getting cached data") # print out message if m is not NULL(has data)
        return(m) # return cached data
    }
    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    m    
    
}
