
## Description

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set=set, get=get, 
             setInverse=setInverse, getInverse=getInverse)
}

## Description

## In case of x$getInverse() returns null computes inversion of matrix 
## and storing the result into cache 
## otherwise returns already existing matrix from cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
          message("getting cached data.")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}

## Examples:

## > x <- rbind(c(4,7),c(2,6))
## > tmp = makeCacheMatrix(x)
## > tmp$get()
## [,1] [,2]
## [1,]    4    7
## [2,]    2    6

## No cache at first run
## > cacheSolve(tmp)
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

## Retrieving from the cache
## > cacheSolve(tmp)
## getting cached data.
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4