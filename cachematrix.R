## These functions utilize lexical scoping to find the inverse of a matrix 

## If the inverse of the matrix has already been calculated previously, it is
## retrieved from the database

## else, the inverse is calculated and cached for future use

## this function creates a matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
     
     a <- NULL
     set <- function(y){
          x <<- y
          a <<- NULL
     }
     get <- function()x
     setInverse <- function(inverse) a <<- inverse
     getInverse <- function() a 
     list(set = set, get = get, 
          setInverse = setInverse, 
          getInverse = getInverse)
}


## This function checks the above special matrix object for the user 
## provided matrix

## if the user provided matrix exists, it returns the already cached inverse of 
## the matrix

## else, it uses the <<- operator and lexical scoping to calculate the inverse
## of the matrix and write it to the above special matrix object


cacheSolve <- function(x, ...) {
     ## searches cache for user provided matrix
     a <- x$getInverse()
     if(!is.null(a)){
          message("getting cached data")
          return(a)
     }
     ## calculates inverse and writes it to cache
     mat <- x$get()
     a <- solve(mat,...)
     x$setInverse(a)
     a
}
