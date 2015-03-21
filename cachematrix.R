## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## 1. takes a matrix as an input
## 2. set assigns the input matrix to a variable
## 3. get returns the matrix as is
## 4. setinverse finds the inverse of the matrix
## 5. getinverse reports the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<-solve
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve
## 1. checks if the inverse of the matrix has already been cached. 
##    If yes - returns the cached value of the matrix inverse
## 2. If not, it newly calculates the matrix inverse and returns the value

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
