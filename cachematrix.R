## Assignment 2 Submission file:
## Student Name: Saeedeh Salimianrizi
## Email: ssalimianrizi@air-worldwide.com
## Secondary Email: saeideh.salimiyan@gmail.com
## these following two functions calculate the inverse of a matrix
## The matrix you pass on to the makeCacheMatrix must be an invertible square matrix!

# this function returns a list. The list is the input to the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {  
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# this function calculates the inverse of the marix.
# if the inverse of a specific matrix has been calculated before ,
# it gets the inverse from the Cache.
# if it is the first time that this matrix is passed on to the function,
# it calculates the inverse using Solve.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    # caching the inverse if it has been calculated before
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    # calculating the inverse for the first time
    data <- x$get()
    message("calculating the inverse") 
    inv <- solve(data)
    x$setinverse(inv)
    inv
}